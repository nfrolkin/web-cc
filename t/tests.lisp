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

(defun random-string (start-char end-char)
  (let* ((char-generator (gen-character :code (gen-integer :min (char-code start-char)
                                                           :max (char-code end-char)))))
    (funcall (gen-string :elements char-generator))))

(defun random-float (bound)
  (funcall (gen-float :bound bound)))


(test test-parse-numbers
  (dolist (cell *test-numbers*)
    (let ((expected (cdr cell))
          (actual (web-cc:parse (car cell) :number)))
      (is (= expected actual)))))

(test test-parse-constant
  (let ((name (random-string #\A #\Z))
        (value (random-float 10)))
    (web-cc:def-constant name value)
    (is (= value (web-cc:parse (string name) :number)))))

(test test-signal-undefined-constant
  (signals web-cc:undefined-constant-error
    (web-cc:parse (random-string #\A #\Z))))

(test test-signal-incorrect-constant-value-in-definition
  (signals type-error
    (web-cc:def-constant (random-string #\A #\Z) nil)))

(test test-signal-incorrect-constant-name-in-expression
  (signals esrap:esrap-parse-error
    (web-cc:parse (random-string #\a #\z))))

(test test-parse-power-operation
  (for-all ((base (gen-float :bound 10))
            (power (gen-float :bound 10) (or (not (zerop base))
                                            (plusp power))))
    (is (= (expt base power)
           (web-cc:parse (format nil " ~$ ^ ~$ " base power) :number)))))

(test test-parse-signed-numbers
  (for-all ((positive-number (gen-integer :min 0 :max 30)))
    (is (= positive-number
           (web-cc:parse (format nil " + ~a " positive-number))))
    (is (= (* -1 positive-number)
           (web-cc:parse (format nil " - ~a " positive-number))))))
