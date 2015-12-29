(cl:in-package #:cluffer-test)

(defun test-simple-buffer-1 ()
  (let* ((line (make-instance 'cluffer-simple-line:line))
	 (buffer (make-instance 'cluffer-simple-buffer:buffer
		   :initial-line line)))
    (assert (= (cluffer:line-count buffer) 1))))

(defun test-simple-buffer ()
  (test-simple-buffer-1))
