(in-package #:web-cc)


(defparameter *top-level-rule* 'power)
(defparameter *defined-constants* (make-hash-table :test #'equal))


;; Conditions
(define-condition undefined-constant-error (error)
  ((constant
    :initarg :constant
    :reader undefined-constant-error-name))
  (:documentation
   "Signaled when parser meet valid constant name without associated value."))

(define-condition invalid-result-type (error)
  ((result-type
    :initarg :result-type
    :reader invalid-result-type-datum))
  (:documentation
   "Signaled when parser function supplied with unknown RESULT-TYPE.
See parser function for more details."))


;; Support classes
(defclass constant-cell ()
  ((documentation
    :reader constant-doc
    :initarg :doc)
   (value
    :reader constant-value
    :initarg :value)))


;; Parser functions
(defun parse-constant (name)
  (if (gethash name *defined-constants*)
      (constant-value (gethash name *defined-constants*))
      (error 'undefined-constant-error :constant name)))

(defun parse-number (num)
  (let ((n (parse-number:parse-real-number num)))
    (if (floatp n) n (float n))))


;; Main functions
(defun parse (expression &optional (result-type :number))
  "Parse EXPRESSION and return result.

Parse EXPRESSION with *TOP-LEVEL-RULE* as start rule.
Result type depends on RESULT-TYPE:
 - :number - evaluate AST and return number;
 - :tree - return AST that is valid Lisp sexp.
If RESULT-TYPE is not equal above types then signals error."
  (case result-type
    (:number (eval (esrap:parse *top-level-rule* expression)))
    (:tree (nth-value 0 (esrap:parse *top-level-rule* expression)))
    (otherwise (error 'invalid-result-type
                      :result-type result-type))))

(defun def-constant (name value &key documentation)
  "Define a new constant or redefine old one.

Define a new constant identified by NAME with value equal to VALUE.
If such constant exist then replace old value with new one.
DOCUMENTATION is used (if supplied) for explain purpose of constant.
Return NIL if constant is new or T for redefinition."
  (check-type value real)
  (let* ((const-name (string-upcase name))
         (new-const (make-instance 'constant-cell
                                   :value value
                                   :doc (when documentation
                                          (string documentation))))
         (redefine-p (nth-value 1 (gethash const-name *defined-constants*))))
    (setf (gethash const-name *defined-constants*) new-const)
    redefine-p))

(defun delete-constant (name)
  "Delete constant with NAME.

Delete constant with NAME from defined constants.
Return T if constant existed, NIL otherwise."
  (remhash (string-upcase name) *defined-constants*))

(defun delete-all-constants ()
  "Delete all constants.

Delete all constants from defined constants. Always return NIL."
  (clrhash *defined-constants*)
  nil)


;; Grammar rules
(defrule digit (character-ranges (#\0 #\9)))

(defrule digits (+ digit))

(defrule whitespaces (+ (or #\Space #\Newline #\Tab))
  (:constant nil))

(defrule lowercase-letter (character-ranges (#\a #\z)))

(defrule uppercase-letter (character-ranges (#\A #\Z)))

(defrule power (and (? whitespaces) base (? whitespaces) (? exponent))
  (:destructure (w1 base w2 exp)
                (declare (ignore w1 w2))
                (if exp
                    (list 'expt base exp)
                    base)))

(defrule exponent (and "^" power)
  (:destructure (raise-op expression)
                (declare (ignore raise-op))
                expression))

(defrule base atom)

(defrule atom (or number constant))

(defrule constant (and (+ uppercase-letter) (* (or digit uppercase-letter)))
  (:text t)
  (:function parse-constant))

(defrule number (or exponentfloat pointfloat digits)
  (:text t)
  (:function parse-number))

(defrule pointfloat (or (and (? digits) "." digits)
                        (and digits ".")))

(defrule exponentfloat (and (or pointfloat digits)
                            (or "e" "E")
                            (? (or "+" "-"))
                            digits))
