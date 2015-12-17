(cl:in-package #:asdf-user)

(defsystem :cluffer-base
  :depends-on (:acclimation)
  :serial t
  :components
  ((:file "packages")
   (:file "edit-protocol")
   (:file "update-protocol")
   (:file "internal-protocol")
   (:file "conditions")
   (:file "condition-reporters-english")
   (:file "check-cursor-attached")
   (:file "check-other-preconditions")
   (:file "methods")))
