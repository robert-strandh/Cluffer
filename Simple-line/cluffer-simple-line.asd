(cl:in-package #:asdf-user)

;;;; This system defines a very simple and very inefficient
;;;; implementation of the line protocol.  It is included here for two
;;;; reasons.  First, we use it to compare results of random
;;;; operations to the same operations on the standard line
;;;; implementation.  Second, it illustrates how to implement the line
;;;; protocol in a simple way, so that clients can use it as a
;;;; starting point for their own implementations of the protocol.

(defsystem :cluffer-simple-line
  :depends-on (:cluffer-base)
  :serial t
  :components
  ((:file "packages")
   (:file "classes")
   (:file "edit-protocol-implementation")))
