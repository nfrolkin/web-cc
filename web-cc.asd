(defsystem "web-cc"
  :depends-on (:esrap
               :parse-number)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "parser")))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :web-cc))))
  (asdf:load-system :web-cc-test)
  (uiop:symbol-call :fiveam :run-all-tests))
