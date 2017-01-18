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

(define-condition mismatch-argument-error (error)
  ((function-name
    :initarg :name
    :reader mismatch-argument-error-function-name)
   (args-provided
    :initarg :args
    :reader mismatch-argument-error-args-provided)
   (args-expected
    :initarg :expect
    :reader mismatch-argument-error-args-expected)))


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
         (new-const (list :value value :doc (when documentation (string documentation))))
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



;; Function utilities
(defun convert-operation (production)
  (let ((operation (case (coerce (first production) 'character)
                     (#\+ '+)
                     (#\- '-)
                     (#\* '*)
                     (#\/ '/)
                     (#\% 'mod)))
        (operand (second production)))
    (cons operation operand)))

(defun process-binary-operation/left-assoc (production)
  (let* ((first-operand (first production))
         (rest-operands (rest production)))
    (if (first rest-operands)
        (loop for (operator . operand) in (first rest-operands)
           with init = first-operand
           for expr = (list operator init operand) then (list operator expr operand)
           finally (return expr))
        first-operand)))


;; Grammar rules
(defrule digit (character-ranges (#\0 #\9)))

(defrule digits (+ digit))

(defrule whitespaces (+ (or #\Space #\Newline #\Tab))
  (:constant nil))

(defrule lowercase-letter (character-ranges (#\a #\z)))

(defrule uppercase-letter (character-ranges (#\A #\Z)))

(defrule sign (or "+" "-"))

(defrule expr (and term (* term-op))
  (:function process-binary-operation/left-assoc))

(defrule term-op (and (or "+" "-") term)
  (:function convert-operation))

(defrule term (and power (* power-op))
  (:function process-binary-operation/left-assoc))

(defrule power-op (and (or "*" "/" "%") power)
  (:function convert-operation))

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
          (getf func-entry :value)
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
    (let ((const-entry (gethash name *defined-constants*)))
      (if const-entry
          (getf const-entry :value)
          (error 'undefined-error :identifier name)))))

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
