(in-package :web-cc)

(defun calculation-handler ()
  (case (hunchentoot:request-method*)
    (:get (calculation-handler/get))
    (:post (calculation-handler/post
            (hunchentoot:post-parameter "expression")))))

(defun calculation-handler/get ()
  (render-template
   "index.html.clt"
   (create-context
    :function-list (list-all-functions)
    :constant-list (list-all-constants))))

(defun calculation-handler/post (expression)
  (let ((context (create-context
                  :expression expression
                  :function-list (list-all-functions)
                  :constant-list (list-all-constants))))
    (handler-case (compute expression)
      (undefined-constant-error (e)
        (setf (getf context :error-type) :undefined-constant
              (getf context :error-message) (undefined-error-identifier e)))
      (undefined-function-error (e)
        (setf (getf context :error-type) :undefined-function
              (getf context :error-message) (undefined-error-identifier e)))
      (mismatch-argument-error (e)
        (setf (getf context :error-type) :mismatch
              (getf context :error-message) (list (mismatch-argument-error-function-name e)
                                                  (mismatch-argument-error-args-expected e)
                                                  (mismatch-argument-error-args-provided e))))
      (parser-error (e)
        (setf (getf context :error-type) :parse
              (getf context :error-message) (parser-error-position e)))
      (error ()
        (setf (getf context :error-type) :calculation))
      (:no-error (answer)
        (setf (getf context :answer) answer)))
    (render-template "index.html.clt" context)))
