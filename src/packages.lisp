(in-package #:common-lisp-user)

(defpackage #:web-cc
  (:use #:common-lisp)
  (:import-from #:esrap
                #:?
                #:defrule
                #:character-ranges)
  (:export #:parse
           #:compute
           #:def-constant
           #:delete-constant
           #:delete-all-constants
           #:undefined-error
           #:undefined-error-identifier))
