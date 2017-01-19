(in-package :web-cc)

(defun calculation-handler ()
  (case (hunchentoot:request-method*)
    (:get (calculation-handler/get))
    (:post (calculation-handler/post
            (hunchentoot:post-parameter "expression")))))

(defun calculation-handler/get ()
  (render-template "index.html.clt"))

(defun calculation-handler/post (expression)
  (declare (ignore expression)))
