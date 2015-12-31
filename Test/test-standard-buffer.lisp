(cl:in-package #:cluffer-test)

(defun check-lines-same (line1 line2)
  (assert (= (cluffer:item-count line1) (cluffer:item-count line2)))
  (assert (equalp (cluffer:items line1) (cluffer:items line1))))

(defun check-buffers-probably-same (buffer1 buffer2)
  (assert (= (cluffer:line-count buffer1) (cluffer:line-count buffer2)))
  (assert (= (cluffer:item-count buffer1) (cluffer:item-count buffer2)))
  (loop for i from 0 below (cluffer:line-count buffer1)
	when (< (random 1.0) 0.1)
	  do (check-lines-same (cluffer:find-line buffer1 i)
			       (cluffer:find-line buffer2 i))))
