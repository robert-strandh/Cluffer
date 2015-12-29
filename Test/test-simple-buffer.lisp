(cl:in-package #:cluffer-test)

(defun test-simple-buffer-1 ()
  (let* ((line (make-instance 'cluffer-simple-line:line))
	 (buffer (make-instance 'cluffer-simple-buffer:buffer
		   :initial-line line)))
    (assert (= (cluffer:line-count buffer) 1))
    (assert (= (cluffer:item-count buffer) 0))))

(defun test-simple-buffer-2 ()
  (let* ((line (make-instance 'cluffer-simple-line:line))
	 (buffer (make-instance 'cluffer-simple-buffer:buffer
		   :initial-line line)))
    (assert (eq line (cluffer:find-line buffer 0)))))

(defun test-simple-buffer ()
  (test-simple-buffer-1)
  (test-simple-buffer-2))
