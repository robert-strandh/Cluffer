(cl:in-package #:common-lisp-user)

(defpackage #:cluffer-standard-line
  (:use #:common-lisp)
  (:export
   #:line
   #:open-line-p
   #:left-sticky-cursor
   #:right-sticky-cursor))
