(cl:in-package #:cluffer)

;;; This condition is signaled when an attempt is made to use a
;;; position that is negative, either by moving a cursor there, or by
;;; attempting to access an item in such a position.
(define-condition beginning-of-line (error acclimation:condition)
  ())

;;; This condition is signaled when an attempt is made to use a
;;; position that is too large, either by moving a cursor there, or by
;;; attempting to access an item in such a position.  Notice that in
;;; some cases, "too large" means "strictly greater than the number of
;;; items in a line", and sometimes it means "greater than or equal to
;;; the number of items in a line".  For example, it is perfectly
;;; acceptable to move a cursor to a position that is equal to the
;;; number of items in a line, but it is not acceptable to attempt to
;;; access an item in a line at that position.
(define-condition end-of-line (error acclimation:condition)
  ())

(define-condition beginning-of-buffer (error acclimation:condition)
  ())

(define-condition end-of-buffer (error acclimation:condition)
  ())

;;; This condition is signaled when an attempt is made to use a cursor
;;; in an operation that requires that cursor to be detached, but the
;;; cursor used in the operation is attached to a line.
(define-condition cursor-attached (error acclimation:condition)
  ())

;;; This condition is signaled when an attempt is made to use a cursor
;;; in an operation that requires that cursor to be attached, but the
;;; cursor used in the operation is not attached to any line.
(define-condition cursor-detached (error acclimation:condition)
  ())
