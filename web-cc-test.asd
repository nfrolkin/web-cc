(defsystem "web-cc-test"
  :depends-on (:web-cc
               :fiveam)
  :pathname "t/"
  :serial t
  :components ((:file "tests")))
