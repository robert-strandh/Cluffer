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
    (assert (eq line (cluffer:find-line buffer 0)))
    (assert (= (cluffer:line-number line) 0))))

(defun test-simple-buffer-3 ()
  (let* ((line (make-instance 'cluffer-simple-line:line))
	 (buffer (make-instance 'cluffer-simple-buffer:buffer
		   :initial-line line)))
    (cluffer:split-line-at-position line 0)
    (assert (= (cluffer:line-count buffer) 2))
    (assert (= (cluffer:item-count buffer) 0))))

(defun test-simple-buffer-4 ()
  (let* ((line (make-instance 'cluffer-simple-line:line))
	 (buffer (make-instance 'cluffer-simple-buffer:buffer
		   :initial-line line)))
    (loop for i from 0 below 10
	  do (cluffer:insert-item-at-position line (1+ i) i))
    (cluffer:split-line-at-position line 5)
    (assert (= (cluffer:line-count buffer) 2))
    (assert (= (cluffer:item-count buffer) 10))
    (assert (equalp (cluffer:items line) #(1 2 3 4 5)))
    (assert (equalp (cluffer:items (cluffer:find-line buffer 1))
		    #(6 7 8 9 10)))
    (cluffer:join-line line)
    (assert (equalp (cluffer:items line) #(1 2 3 4 5 6 7 8 9 10)))))

(defun test-simple-buffer-5 ()
  (let* ((line (make-instance 'cluffer-simple-line:line))
	 (buffer (make-instance 'cluffer-simple-buffer:buffer
		   :initial-line line)))
    (assert-error (cluffer:find-line buffer -1)
		  cluffer:beginning-of-buffer)
    (assert-error (cluffer:find-line buffer 1)
		  cluffer:end-of-buffer)
    (assert-error (cluffer:join-line line)
		  cluffer:end-of-buffer)))

(defun test-simple-buffer-6 ()
  (let ((line (make-instance 'cluffer-simple-line:line)))
    (assert-error (cluffer:line-number line)
		  cluffer:line-detached)
    (assert-error (cluffer:split-line-at-position line 0)
		  cluffer:line-detached)
    (assert-error (cluffer:join-line line)
		  cluffer:line-detached)))

(defun test-simple-buffer ()
  (test-simple-buffer-1)
  (test-simple-buffer-2)
  (test-simple-buffer-3)
  (test-simple-buffer-4)
  (test-simple-buffer-5)
  (test-simple-buffer-6))