(defsystem "web-cc"
  :depends-on (:cl-markup
               :hunchentoot)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "html")))
