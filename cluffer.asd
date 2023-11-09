(cl:in-package #:asdf-user)

(defsystem #:cluffer
  :description "Library providing a protocol for text-editor buffers."
  :author "Robert Strandh <robert.strandh@gmail.com>"
  :license "FreeBSD" ; see file LICENSE.text
  :depends-on (#:cluffer-base
               #:cluffer-standard-line
               #:cluffer-standard-buffer
               #:cluffer-simple-line
               #:cluffer-simple-buffer)
  :in-order-to ((test-op (test-op "cluffer-test"))))
