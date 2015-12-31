(cl:in-package #:cluffer-base)

;;; This :AFTER method calls the function NOTIFY-ITEM-COUNT-CHANGED in
;;; the internal protocol.
(defmethod cluffer:insert-item-at-position :after
    ((line cluffer:line) item position)
  (declare (ignore item position))
  (let ((dock (cluffer-internal:dock line)))
    (cluffer-internal:notify-item-count-changed dock 1)))

;;; This :AFTER method calls the function NOTIFY-ITEM-COUNT-CHANGED in
;;; the internal protocol.
(defmethod cluffer:delete-item-at-position :after
    ((line cluffer:line) position)
  (declare (ignore position))
  (let ((dock (cluffer-internal:dock line)))
    (cluffer-internal:notify-item-count-changed dock -1)))

;;; This :AFTER method calls the function NOTIFY-ITEM-COUNT-CHANGED in
;;; the internal protocol.
(defmethod cluffer:erase-item :after ((cursor cluffer:cursor))
  (let* ((line (cluffer:line cursor))
	 (dock (cluffer-internal:dock line)))
    (cluffer-internal:notify-item-count-changed dock -1)))
