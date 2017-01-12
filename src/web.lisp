(in-package :web-cc)

(defun calculation-handler ()
  (render-page ("/assets/w3.css" "/assets/style.css") ()
               (case (hunchentoot:request-method*)
                 (:post (calculation-handler/post))
                 (:get (calculation-handler/get)))))

(defun calculation-handler/get ())

(defun calculation-handler/post ())
