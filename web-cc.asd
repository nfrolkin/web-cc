(defsystem "web-cc"
  :depends-on (:esrap
               :parse-number)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "parser")))
