(cl:in-package #:cluffer-simple-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Edit protocol

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Method on generic function LINE-COUNT.

(defmethod cluffer:line-count ((buffer buffer))
  (length (contents buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on generic function ITEM-COUNT.

(defmethod cluffer:item-count ((buffer buffer))
  (loop for node across (contents buffer)
	sum (cluffer:item-count (line node))))
