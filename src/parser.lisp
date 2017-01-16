(in-package #:web-cc)


(defparameter *top-level-rule* 'expr)
(defparameter *defined-constants* (make-hash-table :test #'equal))


;; Conditions
(define-condition undefined-constant-error (error)
  ((constant
    :initarg :constant
    :reader undefined-constant-error-name))
  (:documentation
   "Signaled when parser meet valid constant name without associated value."))


;; Support classes
(defclass constant-cell ()
  ((documentation
    :reader constant-doc
    :initarg :doc)
   (value
    :reader constant-value
    :initarg :value)))


;; Main functions
(defun compute (expression)
  "Compute EXPRESSION and return answer as float.

Compute EXPRESSION using parse function and provide
some error handling for convience."
  (realpart (eval (parse expression))))

(defun parse (expression)
  "Parse EXPRESSION and return result.

Parse EXPRESSION with *TOP-LEVEL-RULE* as start rule.
Return Abstract Syntax Tree of EXPRESSION.
This AST is valid Lisp code so it can be evaluated.
EXPRESSION must be type of string or error would be signaled."
  (check-type expression string)
  (nth-value 0 (esrap:parse *top-level-rule* expression)))

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

(defrule sign (or "+" "-"))

(defrule expr power)

(defrule power (and (? whitespaces) signed-base (? whitespaces) (? exponent))
  (:destructure (w1 (sign . base) w2 exp)
                (declare (ignore w1 w2))
                (let ((base (if (string-equal sign "-") (list '- base) base)))
                  (if exp
                      (list 'expt base exp)
                      base))))

(defrule signed-base (and (? sign) (? whitespaces) base)
  (:destructure (sign w1 base)
                (declare (ignore w1))
                (cons sign base)))

(defrule exponent (and "^" power)
  (:destructure (raise-op expression)
                (declare (ignore raise-op))
                expression))

(defrule base atom)

(defrule atom (or number constant))

(defrule constant (and (+ uppercase-letter) (* (or digit uppercase-letter)))
  (:text t)
  (:lambda (name)
    (if (gethash name *defined-constants*)
        (constant-value (gethash name *defined-constants*))
        (error 'undefined-constant-error :constant name))))

(defrule number (or exponentfloat pointfloat digits)
  (:text t)
  (:lambda (num)
    (let ((n (parse-number:parse-real-number num)))
      (if (floatp n) n (float n)))))

(defrule pointfloat (or (and (? digits) "." digits)
                        (and digits ".")))

(defrule exponentfloat (and (or pointfloat digits)
                            (or "e" "E")
                            (? (or "+" "-"))
                            digits))
