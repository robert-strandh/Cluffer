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

(defun random-operation (line1 line2)
  (cond ((and (not (cluffer:last-line-p line1)) (< (random 1.0) 0.001))
	 (cluffer:join-line line1)
	 (cluffer:join-line line2))
	((< (random 1.0) 0.002)
	 (let ((position (random (1+ (cluffer:item-count line1)))))
	   (cluffer:split-line-at-position line1 position)
	   (cluffer:split-line-at-position line2 position)))
	((and (plusp (cluffer:item-count line1)) (< (random 1.0) 0.4))
	 (let ((position (random (cluffer:item-count line1))))
	   (cluffer:delete-item-at-position line1 position)
	   (cluffer:delete-item-at-position line2 position)))
	(t
	 (let ((position (random (1+ (cluffer:item-count line1))))
	       (item (random 100000)))
	   (cluffer:insert-item-at-position line1 item position)
	   (cluffer:insert-item-at-position line2 item position)))))

(defun test-standard-buffer-1 (n)
  (let* ((line1 (make-instance 'cluffer-simple-line:line))
	 (buffer1 (make-instance 'cluffer-simple-buffer:buffer
		    :initial-line line1))
	 (line2 (make-instance 'cluffer-standard-line:open-line))
	 (buffer2 (make-instance 'cluffer-standard-buffer:buffer
		    :initial-line line2)))
    (loop repeat n
	  for line-number = (random (cluffer:line-count buffer1))
	  do (random-operation (cluffer:find-line buffer1 line-number)
			       (cluffer:find-line buffer2 line-number))
	     (check-buffers-probably-same buffer1 buffer2))))
