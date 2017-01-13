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


(test test-parse-numbers
  (dolist (cell *test-numbers*)
    (let ((expected (cdr cell))
          (actual (web-cc:parse (car cell) :number)))
      (is (= expected actual)))))
