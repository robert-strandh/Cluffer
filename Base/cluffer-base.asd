(cl:in-package #:asdf-user)

(defsystem #:cluffer-base
  :depends-on (#:acclimation)
  :serial t
  :components
  ((:file "packages")
   (:file "internal-protocol")
   (:file "edit-protocol")
   (:file "update-protocol")
   (:file "conditions")
   (:file "condition-reporters-english")
   (:file "check-cursor-attached")
   (:file "check-other-preconditions")
   (:file "default-methods")
   (:file "default-internal-methods")
   (:file "methods")))
