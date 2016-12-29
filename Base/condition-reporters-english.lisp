(cl:in-package #:cluffer-base)

(defmethod acclimation:report-condition
    ((condition cluffer:beginning-of-line)
     stream
     (language acclimation:english))
  (format stream "Attempt to access beyond the beginning of a line."))

(defmethod acclimation:report-condition
    ((condition cluffer:end-of-line)
     stream
     (language acclimation:english))
  (format stream "Attempt to access beyond the end of a line."))

(defmethod acclimation:report-condition
    ((condition cluffer:beginning-of-buffer)
     stream
     (language acclimation:english))
  (format stream "Attempt to access beyond the beginning of the buffer."))

(defmethod acclimation:report-condition
    ((condition cluffer:end-of-buffer)
     stream
     (language acclimation:english))
  (format stream "Attempt to access beyond the end of the buffer."))
