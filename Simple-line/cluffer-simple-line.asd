(cl:in-package #:asdf-user)

(defsystem :cluffer-simple-line
  :depends-on (:cluffer-base)
  :serial t
  :components
  ((:file "packages")
   (:file "classes")
   (:file "edit-protocol-implementation")))
