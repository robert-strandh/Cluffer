(cl:in-package #:cluffer-simple-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on CURSOR-ATTACHED-P.

(defmethod cluffer:cursor-attached-p ((cursor cursor))
  (not (null (line cursor))))

(defmethod cluffer:item-count ((line line))
  (length (contents line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on ITEMS.

;;; When all the items are asked for, we do not allocate a fresh
;;; vector.  This means that client code is not allowed to mutate the
;;; return value of this function
(defmethod cluffer:items ((line line) &key (start 0) (end nil))
  (if (and (= start 0) (null end))
      (contents line)
      (subseq (contents line) start end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Detaching and attaching a cursor.

(defmethod cluffer:attach-cursor ((cursor cursor)
				  line
				  &optional
				    (position 0))
  (declare (ignore line position))
  (push cursor (cursors line))
  (setf (line cursor) line)
  (setf (cluffer:cursor-position cursor) position)
  nil)

(defmethod cluffer:detach-cursor ((cursor cursor))
  (setf (cursors (cluffer:line cursor))
	(remove cursor (cursors (cluffer:line cursor))))
  (setf (cluffer:line cursor) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on INSERT-ITEM-AT-POSITION.

(defmethod cluffer:insert-item-at-position ((line line) item position)
  (let ((contents (contents line)))
    (setf (contents line)
	  (concatenate 'vector
		       (subseq contents 0 position)
		       (vector item)
		       (subseq contents position)))
    (loop for cursor in (cursors line)
	  do (when (or (> (cluffer:cursor-position cursor) position)
		       (and (= (cluffer:cursor-position cursor) position)
			    (typep cursor 'right-sticky-mixin)))
	       (incf (cluffer:cursor-position cursor))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on DELETE-ITEM-AT-POSITION.

(defmethod cluffer:delete-item-at-position ((line line) position)
  (let ((contents (contents line)))
    (setf (contents line)
	  (concatenate 'vector
		       (subseq contents 0 position)
		       (subseq contents (1+ position))))
    (loop for cursor in (cursors line)
	  do (when (> (cluffer:cursor-position cursor) position)
	       (decf (cluffer:cursor-position cursor))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on ITEM-AT-POSITION.

(defmethod cluffer:item-at-position ((line line) position)
  (aref (contents line) position))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on CLUFFER-INTERNAL:SPLIT-LINE.

(defmethod cluffer-internal:split-line ((cursor cursor))
  (let* ((pos (cluffer:cursor-position cursor))
	 (line (cluffer:line cursor))
	 (contents (contents line))
	 (new-contents (subseq contents pos))
	 (new-line (make-instance 'line
		     :cursors '()
		     :contents new-contents)))
    (setf (contents line)
	  (subseq (contents line) 0 pos))
    (setf (cursors new-line)
	  (loop for cursor in (cursors line)
		when (or (and (typep cursor 'right-sticky-mixin)
			      (>= (cluffer:cursor-position cursor) pos))
			 (and (typep cursor 'left-sticky-mixin)
			      (> (cluffer:cursor-position cursor) pos)))
		  collect cursor))
    (loop for cursor in (cursors new-line)
	  do (setf (cluffer:line cursor) new-line)
	     (decf (cluffer:cursor-position cursor) pos))
    (setf (cursors line)
	  (set-difference (cursors line) (cursors new-line)))
    new-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on CLUFFER-INTERNAL:JOIN-LINE.

(defmethod cluffer-internal:join-line ((line1 line) (line2 line))
  (loop with length = (length (contents line1))
	for cursor in (cursors line2)
	do (setf (cluffer:line cursor) line1)
	   (incf (cluffer:cursor-position cursor) length))
  (setf (contents line1)
	(concatenate 'vector (contents line1) (contents line2)))
  nil)
