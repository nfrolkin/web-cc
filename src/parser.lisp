(in-package #:web-cc)


(defparameter *top-level-rule* 'number)
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


;; Parser functions
(defun parse-constant (name)
  (if (gethash name *defined-constants*)
      (constant-value (gethash name *defined-constants*))
      (error 'undefined-constant-error :constant name)))

(defun parse-number (num)
  (let ((n (parse-number:parse-real-number num)))
    (if (floatp n) n (float n))))


;; Main functions
(defun parse (expression &optional (result :number))
  (case result
    (:number (eval (esrap:parse *top-level-rule* expression)))
    (:tree (values (esrap:parse *top-level-rule* expression)))))

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
(defrule lowercase-letter (character-ranges (#\a #\z)))
(defrule uppercase-letter (character-ranges (#\A #\Z)))


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
