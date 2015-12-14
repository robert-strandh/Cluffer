(cl:in-package #:common-lisp-user)

(defpackage #:cluffer-standard-line
  (:use #:common-lisp)
  (:export
   #:line
   #:open-line
   #:closed-line
   #:detached-left-sticky-cursor
   #:detached-right-sticky-cursor))
