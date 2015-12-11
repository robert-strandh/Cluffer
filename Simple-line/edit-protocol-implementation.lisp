(cl:in-package #:cluffer-simple-line)

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

(defmethod cluffer:attach-cursor ((cursor attached-cursor)
				  line
				  &optional
				    position)
  (declare (ignore line position))
  (error 'cluffer:cursor-attached))

(defmethod cluffer:attach-cursor ((cursor detached-left-sticky-cursor)
				  (line line)
				  &optional
				    (position 0))
  (when (> position (cluffer:item-count line))
    (error 'cluffer:end-of-line))
  (push cursor (cursors line))
  (change-class cursor 'attached-left-sticky-cursor
		:line line
		:cursor-position position)
  nil)

(defmethod cluffer:attach-cursor ((cursor detached-right-sticky-cursor)
				  (line line)
				  &optional
				    (position 0))
  (when (> position (cluffer:item-count line))
    (error 'cluffer:end-of-line))
  (push cursor (cursors line))
  (change-class cursor 'attached-right-sticky-cursor
		:line line
		:cursor-position position)
  nil)

(defmethod cluffer:detach-cursor ((cursor detached-cursor))
  (error 'cluffer:cursor-detached))

(defmethod cluffer:detach-cursor ((cursor left-sticky-mixin))
  (setf (cursors (line cursor))
	(remove cursor (cursors (line cursor))))
  (change-class cursor 'detached-left-sticky-cursor)
  nil)

(defmethod cluffer:detach-cursor ((cursor right-sticky-mixin))
  (setf (cursors (line cursor))
	(remove cursor (cursors (line cursor))))
  (change-class cursor 'detached-right-sticky-cursor)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Operations on cursors.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on INSERT-ITEM.

(defmethod cluffer:insert-item ((cursor attached-cursor) item)
  (let* ((line (line cursor))
	 (contents (contents line))
	 (position (cluffer:cursor-position cursor)))
    (setf (contents line)
	  (concatenate 'vector
		       (subseq contents 0 position)
		       (vector item)
		       (subseq contents position)))
    (loop for cursor in (cursors line)
	  do (when (typep cursor 'right-sticky-mixin)
	       (incf (cluffer:cursor-position cursor))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on DELETE-ITEM.

(defmethod cluffer:delete-item ((cursor attached-cursor))
  (when (cluffer:end-of-line-p cursor)
    (error 'cluffer:end-of-line))
  (let* ((line (line cursor))
	 (contents (contents line))
	 (position (cluffer:cursor-position cursor)))
    (setf (contents line)
	  (concatenate 'vector
		       (subseq contents 0 position)
		       (subseq contents (1+ position))))
    (loop for cursor in (cursors line)
	  do (when (> (cluffer:cursor-position cursor) position)
	       (decf (cluffer:cursor-position cursor))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on BEGINNING-OF-LINE.
;;;
;;; Position the cursor at the beginning of the line.

(defmethod cluffer:beginning-of-line
    ((cursor attached-cursor))
  (setf (cluffer:cursor-position cursor) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on END-OF-LINE.
;;;
;;; Position the cursor at the end of the line.

(defmethod cluffer:end-of-line
    ((cursor attached-cursor))
  (setf (cluffer:cursor-position cursor)
	(cluffer:item-count (line cursor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on ITEM-BEFORE-CURSOR.

(defmethod cluffer:item-before-cursor
    ((cursor attached-cursor))
  (when (cluffer:beginning-of-line-p cursor)
    (error 'cluffer:beginning-of-line))
  (aref (contents (line cursor))
	(1- (cluffer:cursor-position cursor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on ITEM-AFTER-CURSOR.

(defmethod cluffer:item-after-cursor
    ((cursor attached-cursor))
  (when (cluffer:beginning-of-line-p cursor)
    (error 'cluffer:beginning-of-line))
  (aref (contents (line cursor))
	(cluffer:cursor-position cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on CLUFFER-INTERNAL:SPLIT-LINE.

(defmethod cluffer-internal:split-line ((cursor attached-cursor))
  (let* ((pos (cluffer:cursor-position cursor))
	 (line (line cursor))
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
	  do (setf (line cursor) new-line)
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
	do (setf (line cursor) line1)
	   (incf (cluffer:cursor-position cursor) length))
  (setf (contents line1)
	(concatenate 'vector (contents line1) (contents line2)))
  nil)
