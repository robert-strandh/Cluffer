(cl:in-package #:cluffer-base)

(defmethod print-object ((object cluffer:buffer) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~D line~:P ~D item~:P"
            (cluffer:line-count object)
            (cluffer:item-count object))))

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

(defmethod print-object ((object cluffer:cursor) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (if (cluffer:cursor-attached-p object)
        (format stream "~:D:~:D"
                (cluffer:line-number object)
                (cluffer:cursor-position object))
        (format stream "detached"))))
