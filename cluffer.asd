(cl:in-package #:asdf-user)

(defsystem :cluffer
  :depends-on (:acclimation
	       :clump-binary-tree)
  :serial t
  :components
  ((:file "packages")
   (:file "buffer")
   (:file "update-protocol")
   (:file "line")
   (:file "conditions")
   (:file "condition-reporters-english")))
