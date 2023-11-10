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

(defmethod acclimation:report-condition
    ((condition cluffer:cursor-attached)
     stream
     (language acclimation:english))
  (format stream "Attempt to use an attached cursor~@
                  when a detached cursor was required."))

(defmethod acclimation:report-condition
    ((condition cluffer:cursor-detached)
     stream
     (language acclimation:english))
  (format stream "Attempt to use a detached cursor~@
                  when an attached cursor was required."))

(defmethod acclimation:report-condition
    ((condition cluffer:cursors-are-not-comparable)
     stream
     (language acclimation:english))
  (format stream "~@<Attempt to use compare cursors ~s and ~s ~
                  which are attached to lines which belong to ~
                  different buffers.~@:>"
          (cluffer:cursor1 condition) (cluffer:cursor2 condition)))

(defmethod acclimation:report-condition
    ((condition cluffer:line-detached)
     stream
     (language acclimation:english))
  (format stream "Attempt to use a detached line~@
                  when an attached line was required."))

(defmethod acclimation:report-condition
    ((condition cluffer:object-must-be-line)
     stream
     (language acclimation:english))
  (format stream "Attempt to use an object that is not a line~@
                  in an operation requiring a line object:
                  ~s"
          (object condition)))

(defmethod acclimation:report-condition
    ((condition cluffer:object-must-be-buffer)
     stream
     (language acclimation:english))
  (format stream "Attempt to use an object that is not a buffer~@
                  in an operation requiring a buffer object:~@
                  ~s"
          (object condition)))
