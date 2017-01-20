(in-package #:web-cc)

(defparameter *assets-dispatcher*
  (hunchentoot:create-folder-dispatcher-and-handler
   "/assets/" (merge-pathnames "assets/" (asdf:system-source-directory :web-cc))))
(defparameter *calculator-dispatcher*
  (hunchentoot:create-regex-dispatcher "^/$" #'calculation-handler))
(defparameter *current-acceptor* nil)


(defun simple-server/run (port)
  (pushnew *assets-dispatcher* hunchentoot:*dispatch-table*)
  (pushnew *calculator-dispatcher* hunchentoot:*dispatch-table*)
  (setf *current-acceptor* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                                             :port port))))

(defun simple-server/stop (&optional (soft-stop t))
  (when *current-acceptor*
    (hunchentoot:stop *current-acceptor*
                      :soft soft-stop)
    (setf *current-acceptor* nil)))
