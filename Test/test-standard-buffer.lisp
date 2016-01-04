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

(defparameter *operations* '())

(defparameter *recording-p* nil)

(defun record (operation)
  (when *recording-p*
    (format t "~s~%" operation)
    (push operation *operations*)))

(defun join (line1 line2 line-number)
  (record `(join ,line-number))
  (cluffer:join-line line1)
  (cluffer:join-line line2))

(defun split (line1 line2 line-number position)
  (record `(split ,line-number ,position))
  (cluffer:split-line-at-position line1 position)
  (cluffer:split-line-at-position line2 position))

(defun random-split (line1 line2 line-number)
  (let ((position (random (1+ (cluffer:item-count line1)))))
    (split line1 line2 line-number position)))

(defun insert (line1 line2 line-number position item)
  (record `(insert ,line-number ,position ,item))
  (cluffer:insert-item-at-position line1 item position)
  (cluffer:insert-item-at-position line2 item position))

(defun random-insert (line1 line2 line-number)
  (let ((position (random (1+ (cluffer:item-count line1))))
	(item (random 100000)))
    (insert line1 line2 line-number position item)))

(defun delete (line1 line2 line-number position)
  (record `(delete ,line-number ,position))
  (cluffer:delete-item-at-position line1 position)
  (cluffer:delete-item-at-position line2 position))

(defun random-delete (line1 line2 line-number)
  (let ((position (random (cluffer:item-count line1))))
    (delete line1 line2 line-number position)))

(defun random-operation (line1 line2 line-number)
  (cond ((and (not (cluffer:last-line-p line1)) (< (random 1.0) 0.001))
	 (join line1 line2 line-number)
	 (values -1 0))
	((< (random 1.0) 0.002)
	 (random-split line1 line2 line-number)
	 (values 1 0))
	((and (plusp (cluffer:item-count line1)) (< (random 1.0) 0.4))
	 (random-delete line1 line2 line-number)
	 (values 0 -1))
	(t
	 (random-insert line1 line2 line-number)
	 (values 0 1))))

(defun test-standard-buffer-1 (n)
  (setf *operations* '())
  (let* ((line1 (make-instance 'cluffer-simple-line:line))
	 (buffer1 (make-instance 'cluffer-simple-buffer:buffer
		    :initial-line line1))
	 (line2 (make-instance 'cluffer-standard-line:open-line))
	 (buffer2 (make-instance 'cluffer-standard-buffer:buffer
		    :initial-line line2))
	 (lc 1)
	 (ic 0))
    (loop repeat n
	  for line-number = (random (cluffer:line-count buffer1))
	  do (multiple-value-bind (dl di)
		 (random-operation (cluffer:find-line buffer1 line-number)
				   (cluffer:find-line buffer2 line-number)
				   line-number)
	       (incf lc dl)
	       (incf ic di)
	       (assert (= (cluffer:line-count buffer1) lc))
	       (assert (= (cluffer:line-count buffer2) lc))
	       (assert (= (cluffer:item-count buffer1) ic))
	       (assert (= (cluffer:item-count buffer2) ic))))))

(defun replay (operations)
  (let* ((line1 (make-instance 'cluffer-simple-line:line))
	 (buffer1 (make-instance 'cluffer-simple-buffer:buffer
		    :initial-line line1))
	 (line2 (make-instance 'cluffer-standard-line:open-line))
	 (buffer2 (make-instance 'cluffer-standard-buffer:buffer
		    :initial-line line2)))
    (loop for (name line-number . arguments) in operations
	  for line1 = (cluffer:find-line buffer1 line-number)
	  for line2 = (cluffer:find-line buffer2 line-number)
	  do (apply name line1 line2 line-number arguments))
    (values buffer1 buffer2)))

(defun test-standard-buffer ()
  (loop repeat 30
	do (test-standard-buffer-1 30000)))
