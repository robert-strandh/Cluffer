(cl:in-package #:asdf-user)

(defsystem :cluffer
  :depends-on (:clump-splay-tree)
  :serial t
  :components
  ((:file "packages")
   (:file "buffer")
   (:file "update-protocol")
   (:file "line")))
