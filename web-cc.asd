(defsystem "web-cc"
  :depends-on (:cl-markup)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "html")))
