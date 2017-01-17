(in-package #:web-cc)


(defparameter *top-level-rule* 'expr)
(defparameter *defined-constants* (make-hash-table :test #'equal))
(defparameter *defined-functions* (make-hash-table :test #'equal))


;; Conditions
(define-condition undefined-error (error)
  ((identifier
    :initarg :identifier
    :reader undefined-error-identifier))
  (:documentation
   "Signaled when parser meet valid name but it's undefined."))


;; Support classes
(defclass defined ()
  ((documentation
    :reader defined-info
    :initarg :info)
   (value
    :reader defined-value
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
         (new-const (make-instance 'defined
                                   :value value
                                   :info (when documentation
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

(defrule expr (and term (* term-op))
  (:lambda (production)
    (let* ((term (first production))
           (term-op-multiple (rest production)))
      (if (first term-op-multiple)
          (loop for (operator . operand) in (first term-op-multiple)
             with init = term
             for expr = (list operator init operand) then (list operator expr operand)
             finally (return expr))
          term))))

(defrule term-op (and (or "+" "-") term)
  (:lambda (production)
    (let ((operation (case (coerce (first production) 'character)
                       (#\+ '+)
                       (#\- '-)))
          (term (second production)))
      (cons operation term))))

(defrule term (and power (* power-op))
  (:lambda (production)
    (let* ((power (first production))
           (power-op-multiple (rest production)))
      (if (first power-op-multiple)
          (loop for (operator . operand) in (first power-op-multiple)
             with init = power
             for expr = (list operator init operand) then (list operator expr operand)
             finally (return expr))
          power))))

(defrule power-op (and (or "*" "/" "%") power)
  (:lambda (production)
    (let ((operation (case (coerce (first production) 'character)
                       (#\* '*)
                       (#\/ '/)
                       (#\% 'mod)))
          (power (second production)))
      (cons operation power))))

(defrule power (and (? whitespaces) signed-base (? whitespaces) (? exponent))
  (:lambda (production)
    (let* ((signed-base (second production))
           (exponent (fourth production)))
      (if exponent
          (list 'expt signed-base exponent)
          signed-base))))

(defrule signed-base (and (? sign) (? whitespaces) base)
  (:lambda (production)
    (let* ((sign (first production))
           (base (third production)))
      (if (string= "-" sign)
          (list '- base)
          base))))

(defrule exponent (and "^" power)
  (:lambda (production)
    (second production)))

(defrule base (or function-expr enclosed-expr atom))

(defrule function-expr (and function "(" (? arglist) ")")
  (:lambda (production)
    (let ((function (first production))
          (arglist (third production)))
      (cons function arglist))))

(defrule function (and (+ lowercase-letter) (* (or digit lowercase-letter)))
  (:text t)
  (:lambda (func)
    (let ((func-entry (gethash func *defined-functions*)))
      (if func-entry
          (defined-value func-entry)
          (error 'undefined-error :identifier func)))))

(defrule arglist (and expr (* expr-rest))
  (:lambda (production)
    (let ((first-arg (first production))
          (rest-args (second production)))
      (cons first-arg rest-args))))

(defrule expr-rest (and "," expr)
  (:lambda (production)
    (second production)))

(defrule enclosed-expr (and "(" expr ")")
  (:lambda (production)
    (second production)))

(defrule atom (or number constant))

(defrule constant (and (+ uppercase-letter) (* (or digit uppercase-letter)))
  (:text t)
  (:lambda (name)
    (if (gethash name *defined-constants*)
        (defined-value (gethash name *defined-constants*))
        (error 'undefined-error :identifier name))))

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
