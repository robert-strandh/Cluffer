(cl:in-package #:cluffer-buffer)

(defmethod acclimation:report-condition
    ((condition beginning-of-line)
     stream
     (language acclimation:english))
  (format stream "Attempt to move beyond the beginning of a line."))

(defmethod acclimation:report-condition
    ((condition end-of-line)
     stream
     (language acclimation:english))
  (format stream "Attempt to move beyond the end of a line."))
