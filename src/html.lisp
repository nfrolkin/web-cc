(in-package #:web-cc)

(defparameter *templates-dir* "../templates/")


(defun render-template (template-name &optional context)
  (with-open-file (template-file (merge-pathnames template-name
                                                  *templates-dir*))
    (let ((template (make-string (file-length template-file))))
      (read-sequence template template-file)
      (funcall (cl-template:compile-template template)
               (if context context (create-context))))))

(defun create-context (&key (answer "") (expression "") error-message error-type
                         function-list constant-list)
  (list :answer answer
        :expression expression
        :error-message error-message
        :error-type error-type
        :function-list function-list
        :constant-list constant-list))
