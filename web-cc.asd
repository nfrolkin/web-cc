(defsystem "web-cc"
  :depends-on (:esrap
               :parse-number
               :cl-template
               :hunchentoot)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "parser")
               (:file "html")
               (:file "web")
               (:file "runner")))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :web-cc))))
  (asdf:load-system :web-cc-test)
  (uiop:symbol-call :fiveam :run-all-tests))
