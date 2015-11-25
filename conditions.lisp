(cl:in-package #:cluffer-buffer)

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

