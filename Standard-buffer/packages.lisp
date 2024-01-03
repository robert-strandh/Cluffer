(cl:in-package #:common-lisp-user)

(defpackage #:cluffer-standard-buffer
  (:use #:common-lisp)

  (:local-nicknames
   (#:bt #:clump-binary-tree))

  (:export #:buffer))
