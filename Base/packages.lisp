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
   #:item-at-position
   #:item-before-cursor
   #:item-after-cursor
   #:insert-item-at-position
   #:delete-item-at-position
   #:insert-item
   #:delete-item
   #:erase-item
   #:beginning-of-buffer
   #:end-of-buffer
   #:cursor-attached
   #:cursor-detached
   #:line-detached
   #:object-must-be-line
   #:beginning-of-buffer-p
   #:end-of-buffer-p
   #:split-line-at-position
   #:split-line
   #:join-line
   #:items
   #:attach-cursor
   #:detach-cursor
   #:line-number
   #:find-line
   #:first-line-p
   #:last-line-p
   #:current-time
   #:update))

(defpackage #:cluffer-internal
  (:use #:common-lisp)
  (:export #:line-split-line
	   #:join-line
	   #:dock
	   #:buffer-line-number
	   #:buffer-split-line
	   #:notify-item-count-changed))

(defpackage #:cluffer-base
  (:use #:common-lisp)
  (:export))
