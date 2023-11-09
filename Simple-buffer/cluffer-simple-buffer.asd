(cl:in-package #:asdf-user)

(defsystem #:cluffer-simple-buffer
  :long-description "This system defines a very simple and very inefficient
implementation of the buffer protocol.  It is included here for two
reasons.  First, we use it to compare results of random
operations to the same operations on the standard buffer
implementation.  Second, it illustrates how to implement the buffer
protocol in a simple way, so that clients can use it as a
starting point for their own implementations of the protocol."
  :depends-on (#:cluffer-base)
  :serial t
  :components
  ((:file "packages")
   (:file "classes")
   (:file "edit-protocol-implementation")
   (:file "update-protocol-implementation")))
