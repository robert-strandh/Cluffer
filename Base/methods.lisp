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

(declaim (inline check-same-buffer))
(defun check-same-buffer (line1 line2 cursor1 cursor2)
  (unless (eq (cluffer:buffer line1) (cluffer:buffer line2))
    (error 'cluffer:cursors-are-not-comparable :cursor1 cursor1
                                               :cursor2 cursor2)))

(defmethod cluffer:cursor</2 ((cursor1 cluffer:cursor)
                              (cursor2 cluffer:cursor))
  (let ((line1 (cluffer:line cursor1))
        (line2 (cluffer:line cursor2)))
    (check-same-buffer line1 line2 cursor1 cursor2)
    (or (and (eq line1 line2)
             (< (cluffer:cursor-position cursor1)
                (cluffer:cursor-position cursor2)))
        (< (cluffer:line-number line1) (cluffer:line-number line2)))))

(defmethod cluffer:cursor<=/2 ((cursor1 cluffer:cursor)
                               (cursor2 cluffer:cursor))
  (let ((line1 (cluffer:line cursor1))
        (line2 (cluffer:line cursor2)))
    (check-same-buffer line1 line2 cursor1 cursor2)
    (or (and (eq line1 line2)
             (<= (cluffer:cursor-position cursor1)
                 (cluffer:cursor-position cursor2)))
        (< (cluffer:line-number line1) (cluffer:line-number line2)))))

(defmethod cluffer:cursor=/2 ((cursor1 cluffer:cursor)
                              (cursor2 cluffer:cursor))
  (let ((line1 (cluffer:line cursor1))
        (line2 (cluffer:line cursor2)))
    (check-same-buffer line1 line2 cursor1 cursor2)
    (and (eq line1 line2)
         (= (cluffer:cursor-position cursor1)
            (cluffer:cursor-position cursor2)))))
