(cl:in-package #:cluffer-internal)

(defmethod dock-line-number ((dock null) (line cluffer:line))
  (error 'cluffer:line-detached))

(defmethod dock-line-number ((dock dock) (line cluffer:line))
  (buffer-line-number (buffer dock) dock line))

(defmethod dock-split-line ((dock null) (line cluffer:line) position)
  (declare (ignore position))
  (error 'cluffer:line-detached))

(defmethod dock-split-line ((dock dock) (line cluffer:line) position)
  (buffer-split-line (buffer dock) dock line position))

(defmethod dock-join-line ((dock null) (line cluffer:line))
  (error 'cluffer:line-detached))

(defmethod dock-join-line ((dock dock) (line cluffer:line))
  (buffer-join-line (buffer dock) dock line))

;;; Default method on NOTIFY-ITEM-COUNT-CHANGED.  This method does
;;; nothing, and is used when a line is not attached to a buffer.
(defmethod notify-item-count-changed ((dock null) delta)
  (declare (ignore delta))
  nil)
