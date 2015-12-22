(cl:in-package #:cluffer-internal)

(defmethod dock-line-number ((dock null) (line cluffer:line))
  (error 'cluffer:line-detached))

(defmethod dock-line-number ((dock dock) (line cluffer:line))
  (buffer-line-number (buffer dock) dock line))
