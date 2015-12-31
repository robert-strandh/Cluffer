(cl:in-package #:common-lisp-user)

(defpackage #:cluffer
  (:use)
  (:export
   #:buffer
   #:line
   #:cursor
   #:item-count
   #:cursor-position
   #:beginning-of-line-p
   #:end-of-line-p
   #:forward-item
   #:backward-item
   #:beginning-of-line
   #:end-of-line
   #:item-before-cursor
   #:item-after-cursor
   #:insert-item
   #:delete-item
   #:erase-item
   #:update))
