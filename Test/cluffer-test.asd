(cl:in-package #:asdf-user)

(defsystem :cluffer-test
  :depends-on (:cluffer)
  :serial t
  :components
  ((:file "packages")
   (:file "test-simple-line")
   (:file "test")))
