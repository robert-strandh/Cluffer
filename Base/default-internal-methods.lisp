(cl:in-package #:cluffer-internal)

(defmethod dock-line-number ((dock null) (line cluffer:line))
  (error 'cluffer:line-detached))

(defmethod dock-line-number ((dock dock) (line cluffer:line))
  (buffer-line-number (buffer dock) dock line))

(defmethod dock-split-line ((dock null) (line cluffer:line) position)
  (declare (ignore position))
  (error 'cluffer:line-detached))

(defmethod dock-split-lnie ((dock dock) (line cluffer:line) position)
  (buffer-split-line (buffer dock) dock line position))
