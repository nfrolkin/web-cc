(in-package #:common-lisp-user)

(defpackage #:web-cc.test
  (:use #:common-lisp
        #:fiveam))
(in-package #:web-cc.test)

(defparameter *test-numbers* '(("3.14" . 3.14)
                               ("10." . 10.)
                               (".001" . .001)
                               ("1e10" . 1e10)
                               ("3.14e-10" . 3.14e-10)
                               ("0e0" . 0e0)))
(defparameter *test-term-ops* (list (cons "*" '*)
                                    (cons "/" '/)
                                    (cons "%" 'mod)))
(defparameter *test-expr-ops* (list (cons "+" '+)
                                    (cons "-" '-)))

(defun random-string (start-char end-char)
  (let* ((char-generator (gen-character :code (gen-integer :min (char-code start-char)
                                                           :max (char-code end-char)))))
    (funcall (gen-string :elements char-generator))))

(defun random-float (bound)
  (funcall (gen-float :bound bound)))


(test test-parse-numbers
  (dolist (cell *test-numbers*)
    (let ((expected (cdr cell))
          (actual (web-cc:compute (car cell))))
      (is (= expected actual)))))

(test test-parse-constant
  (let ((name (random-string #\A #\Z))
        (value (random-float 10)))
    (web-cc:def-constant name value)
    (is (= value (web-cc:parse (string name))))))

(test test-signal-undefined-constant
  (signals web-cc:undefined-error
    (web-cc:parse (random-string #\A #\Z))))

(test test-signal-incorrect-constant-value-in-definition
  (signals type-error
    (web-cc:def-constant (random-string #\A #\Z) nil)))

(test test-signal-incorrect-constant-name-in-expression
  (signals esrap:esrap-parse-error
    (web-cc:parse (random-string #\a #\z))))

(test test-parse-power-operation
    (is (equal (list 'expt 1.0 1.0)
               (web-cc:parse "1.0 ^ 1.0"))))

(test test-parse-signed-numbers
    (is (equal (list '- 1.0) (web-cc:parse "-1.0")))
    (is (= 1.0 (web-cc:parse "+1.0"))))

(test test-parse-enclosed-expression
  (is (equal (list 'expt (list 'expt 1.0 1.0) 2.0)
             (web-cc:parse "(1.0 ^ 1.0) ^ 2.0"))))

(test test-parse-term
  (dolist (cell *test-term-ops*)
    (is (equal (list (cdr cell) 1.0 1.0)
               (web-cc:parse (format nil "~$ ~a ~$"
                                     1.0 (car cell) 1.0))))))

(test test-parse-expr
  (dolist (cell *test-expr-ops*)
    (is (equal (list (cdr cell) 1.0 1.0)
               (web-cc:parse (format nil "~$ ~a ~$"
                                     1.0 (car cell) 1.0))))))
