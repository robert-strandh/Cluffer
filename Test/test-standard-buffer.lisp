(cl:in-package #:cluffer-test)

(defun check-lines-same (line1 line2)
  (assert (= (cluffer:item-count line1) (cluffer:item-count line2)))
  (assert (equalp (cluffer:items line1) (cluffer:items line1))))
