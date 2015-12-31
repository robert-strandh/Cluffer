(cl:in-package #:asdf-user)

(defsystem :cluffer-test
  :depends-on (:cluffer)
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "test-simple-line")
   (:file "test-standard-line")
   (:file "test-simple-buffer")
   (:file "test-standard-buffer")
   (:file "test")))
