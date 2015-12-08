(cl:in-package #:common-lisp-user)

(defpackage #:cluffer
  (:use #:common-lisp)
  (:export
   #:buffer
   #:line
   #:cursor
   #:line-count
   #:item-count
   #:cursor-attached-p
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
   #:beginning-of-buffer
   #:end-of-buffer
   #:cursor-attached
   #:cursor-detached
   #:beginning-of-buffer-p
   #:end-of-buffer-p
   #:split-line
   #:join-line
   #:items
   #:attach-cursor
   #:detach-cursor
   #:line-number
   #:first-line-p))

(defpackage #:cluffer-internal
  (:use #:common-lisp)
  (:export #:split-line #:join-line))

(defpackage #:cluffer-base
  (:use #:common-lisp)
  (:export))
