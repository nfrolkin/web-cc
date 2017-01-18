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
    (string (funcall (gen-string :elements char-generator)))))

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
    (is (= value (web-cc:parse name)))))

(test test-signal-undefined-constant
  (signals web-cc:undefined-constant-error
    (web-cc:parse (random-string #\A #\Z))))

(test test-signal-incorrect-constant-value-in-definition
  (signals type-error
    (web-cc:def-constant (random-string #\A #\Z) nil)))

(test test-signal-incorrect-constant-name-in-expression
  (signals web-cc:parser-error
    (web-cc:parse (random-string #\a #\z))))

(test test-parse-function
  (let ((name (random-string #\a #\z))
        (func 'sin))
    (web-cc:def-function name func 1)
    (is (equal (list 'sin 1.0)
               (web-cc:parse (format nil "~a(1.0)" name))))))

(test test-signal-undefined-function
  (signals web-cc:undefined-function-error
    (web-cc:parse (format nil "~a(1.0)" (random-string #\a #\z)))))

(test test-signal-mismatch-arguments-less-when-expected
  (web-cc:def-function "sin" 'sin 1)
  (signals web-cc:mismatch-argument-error
    (web-cc:parse "sin()")))

(test test-signal-mismatch-arguments-more-when-expected
  (web-cc:def-function "sin" 'sin 1)
  (signals web-cc:mismatch-argument-error
    (web-cc:parse "sin(1.0, 1.0)")))

(test test-signal-incorrect-function-value-in-definition
  (signals type-error
    (web-cc:def-function (random-string #\a #\z) "" 1)))

(test test-signal-incorrect-function-name-in-expression
  (signals web-cc:parser-error
    (web-cc:parse (format nil "~a(1.0)" (random-string #\A #\Z)))))

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

(test test-valid-expression
  (web-cc:def-constant "PI" 3.14)
  (web-cc:def-function "sin" 'sin 1)
  (is (equal (list '*
                   (list '+
                         3.14
                         (list 'sin 1.0))
                   (list 'expt
                         4.0
                         (list '- 2.0)))
             (web-cc:parse "(PI + sin(1)) * 4 ^ -2"))))

(test test-invalid-expressions
  (signals web-cc:parser-error
    (web-cc:parse "pi + pi"))
  (signals web-cc:parser-error
    (web-cc:parse "PI()"))
  (signals web-cc:parser-error
    (web-cc:parse "PI(1,2)")))
