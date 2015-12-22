(cl:in-package #:cluffer)

;;; This condition is signaled when an attempt is made to use a
;;; position that is negative, either by moving a cursor there, or by
;;; attempting to access an item in such a position.
(define-condition beginning-of-line (error acclimation:condition)
  ())

(define-condition end-of-line (error acclimation:condition)
  ())

(define-condition beginning-of-buffer (error acclimation:condition)
  ())

(define-condition end-of-buffer (error acclimation:condition)
  ())

(define-condition cursor-attached (error acclimation:condition)
  ())

(define-condition cursor-detached (error acclimation:condition)
  ())

