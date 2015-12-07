(cl:in-package #:asdf-user)

(defsystem :cluffer-base
  :depends-on (:acclimation)
  :serial t
  :components
  ((:file "packages")
   (:file "edit-protocol")
   (:file "conditions")
   (:file "condition-reporters-english")
   (:file "methods")))
