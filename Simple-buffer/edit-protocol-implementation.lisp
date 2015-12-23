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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on generic function FIND-LINE.

(defmethod cluffer:find-line ((buffer buffer) line-number)
  (line (aref (contents buffer) line-number)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on internal generic function BUFFER-LINE-NUMBER.

(defmethod cluffer-internal:buffer-line-number
    ((buffer buffer) (dock node) line)
  (position line (contents buffer) :test #'eq :key #'line))
