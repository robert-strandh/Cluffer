(cl:in-package #:cluffer-base)

(defmethod cluffer:find-line :before ((buffer cluffer:buffer) line-number)
  (when (minusp line-number)
    (error 'cluffer:beginning-of-buffer))
  (when (>= line-number (cluffer:line-count buffer))
    (error 'cluffer:end-of-buffer)))
