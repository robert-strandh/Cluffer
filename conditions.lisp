(cl:in-package #:cluffer-buffer)

(define-condition beginning-of-line (error)
  ())

(define-condition end-of-line (error)
  ())

(define-condition beginning-of-buffer (error)
  ())

(define-condition end-of-buffer (error)
  ())

(define-condition cursor-attached (error)
  ())

(define-condition cursor-detached (error)
  ())

