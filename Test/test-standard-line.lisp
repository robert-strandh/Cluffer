(cl:in-package #:cluffer-test)

(defun random-position (cursor)
  (setf (cluffer:cursor-position cursor)
	(random (1+ (cluffer:item-count cursor)))))
