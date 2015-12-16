(cl:in-package #:cluffer-simple-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Edit protocol

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Method on generic function LINE-COUNT.

(defmethod cluffer:line-count ((buffer buffer))
  (length (contents buffer)))

