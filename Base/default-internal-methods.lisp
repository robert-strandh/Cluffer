(cl:in-package #:cluffer-internal)

(defmethod dock-line-number ((dock dock) (line cluffer:line))
  (buffer-line-number (buffer dock) dock line))
