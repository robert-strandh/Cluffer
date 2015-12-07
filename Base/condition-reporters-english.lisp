(cl:in-package #:cluffer-base)

(defmethod acclimation:report-condition
    ((condition cluffer:beginning-of-line)
     stream
     (language acclimation:english))
  (format stream "Attempt to move beyond the beginning of a line."))

(defmethod acclimation:report-condition
    ((condition cluffer:end-of-line)
     stream
     (language acclimation:english))
  (format stream "Attempt to move beyond the end of a line."))
