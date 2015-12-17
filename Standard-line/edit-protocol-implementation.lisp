(cl:in-package #:cluffer-standard-line)

(defmethod cluffer:item-count ((line open-line))
  (- (length (contents line)) (- (gap-end line) (gap-start line))))

(defmethod cluffer:item-count ((line closed-line))
  (length (contents line)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on ITEMS.

;;; When the items of an open line are asked for, we first close the
;;; line.  While this way of doing it might seem wasteful, it probably
;;; is not that bad.  When the items are asked for, the reason is
;;; probably that those items are going to be displayed or used to
;;; drive a parser, or something else that will imply some significant
;;; work for each item.  So even if the line is repeatedly opened (to
;;; edit) and closed (to display), it probably does not matter much.
;;; A slight improvement could be to leave the line open and return a
;;; freshly allocated vector with the items in it.
(defmethod cluffer:items ((line open-line) &key (start 0) (end nil))
  (close-line line)
  (cluffer:items line :start start :end end))

;;; When all the items are asked for, we do not allocate a fresh
;;; vector.  This means that client code is not allowed to mutate the
;;; return value of this function
(defmethod cluffer:items ((line closed-line) &key (start 0) (end nil))
  (if (and (= start 0) (null end))
      (contents line)
      (subseq (contents line) start end)))

(defgeneric close-cursor (cursor))

(defmethod close-cursor ((cursor open-left-sticky-cursor))
  (change-class cursor 'closed-left-sticky-cursor))
  
(defmethod close-cursor ((cursor open-right-sticky-cursor))
  (change-class cursor 'closed-right-sticky-cursor))

(defgeneric close-line (line))

(defmethod close-line ((line closed-line))
  nil)

(defmethod close-line ((line open-line))
  (mapc #'close-cursor (cursors line))
  (let* ((item-count (cluffer:item-count line))
	 (contents (contents line))
	 (new-contents (make-array item-count)))
    (replace new-contents contents
	     :start1 0 :start2 0 :end2 (gap-start line))
    (replace new-contents contents
	     :start1 (gap-start line) :start2 (gap-end line))
    (change-class line 'closed-line
		  :contents new-contents)
    nil))

(defgeneric open-cursor (cursor))

(defmethod open-cursor ((cursor closed-left-sticky-cursor))
  (change-class cursor 'open-left-sticky-cursor))
  
(defmethod open-cursor ((cursor closed-right-sticky-cursor))
  (change-class cursor 'open-right-sticky-cursor))

(defgeneric open-line (line))

(defmethod open-line ((line open-line))
  nil)

(defmethod open-line ((line closed-line))
  (mapc #'open-cursor (cursors line))
  (let* ((contents (contents line))
	 (item-count (length contents))
	 (new-length (max 32 item-count))
	 (new-contents (make-array new-length)))
    (replace new-contents contents
	     :start1 (- new-length item-count) :start2 0)
    (change-class line 'open-line
		  :contents new-contents
		  :gap-start 0
		  :gap-end (- new-length item-count))
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Detaching and attaching a cursor.

(defmethod cluffer:attach-cursor
    ((cursor attached-cursor) line &optional position)
  (declare (ignore line position))
  (error 'cluffer:cursor-attached))

(defmethod cluffer:attach-cursor
    ((cursor detached-left-sticky-cursor)
     (line open-line)
     &optional
       (position 0))
  (when (> position (cluffer:item-count line))
    (error 'cluffer:end-of-line))
  (push cursor (cursors line))
  (change-class cursor 'open-left-sticky-cursor
		:line line
		:cursor-position position)
  nil)

(defmethod cluffer:attach-cursor
    ((cursor detached-left-sticky-cursor)
     (line closed-line)
     &optional
       (position 0))
  (when (> position (cluffer:item-count line))
    (error 'cluffer:end-of-line))
  (push cursor (cursors line))
  (change-class cursor 'closed-left-sticky-cursor
		:line line
		:cursor-position position)
  nil)
  
(defmethod cluffer:attach-cursor
    ((cursor detached-right-sticky-cursor)
     (line open-line)
     &optional
       (position 0))
  (when (> position (cluffer:item-count line))
    (error 'cluffer:end-of-line))
  (push cursor (cursors line))
  (change-class cursor 'open-right-sticky-cursor
		:line line
		:cursor-position position)
  nil)

(defmethod cluffer:attach-cursor
    ((cursor detached-right-sticky-cursor)
     (line closed-line)
     &optional
       (position 0))
  (when (> position (cluffer:item-count line))
    (error 'cluffer:end-of-line))
  (push cursor (cursors line))
  (change-class cursor 'closed-right-sticky-cursor
		:line line
		:cursor-position position)
  nil)

(defmethod cluffer:detach-cursor
    ((cursor detached-cursor))
  (error 'cluffer:cursor-detached))

(defmethod cluffer:detach-cursor
  ((cursor left-sticky-mixin))
  (setf (cursors (cluffer:line cursor))
	(remove cursor (cursors (cluffer:line cursor))))
  (change-class cursor 'detached-left-sticky-cursor)
  nil)

(defmethod cluffer:detach-cursor
  ((cursor right-sticky-mixin))
  (setf (cursors (cluffer:line cursor))
	(remove cursor (cursors (cluffer:line cursor))))
  (change-class cursor 'detached-right-sticky-cursor)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on INSERT-ITEM-AT-POSITION.

(defmethod cluffer:insert-item-at-position ((line closed-line) item position)
  (open-line line)
  (cluffer:insert-item-at-position line item position))

(defmethod cluffer:insert-item-at-position ((line open-line) item position)
  (let ((contents (contents line)))
    (cond ((= (gap-start line) (gap-end line))
	   (let* ((new-length (* 2 (length contents)))
		  (diff (- new-length (length contents)))
		  (new-contents (make-array new-length)))
	     (replace new-contents contents
		      :start2 0 :start1 0 :end2 position)
	     (replace new-contents contents
		      :start2 position :start1 (+ position diff))
	     (setf (gap-start line) position)
	     (setf (gap-end line) (+ position diff))
	     (setf (contents line) new-contents)))
	  ((< position (gap-start line))
	   (decf (gap-end line) (- (gap-start line) position))
	   (replace contents contents
		    :start2 position :end2 (gap-start line)
		    :start1 (gap-end line))
	   (setf (gap-start line) position))
	  ((> position (gap-start line))
	   (replace contents contents
		    :start2 (gap-end line)
		    :start1 (gap-start line) :end1 position)
	   (incf (gap-end line) (- position (gap-start line)))
	   (setf (gap-start line) position))
	  (t
	   nil))
    (setf (aref (contents line) (gap-start line)) item)
    (incf (gap-start line))
    (loop for cursor in (cursors line)
	  do (when (or (> (cluffer:cursor-position cursor) position)
		       (and (= (cluffer:cursor-position cursor) position)
			    (typep cursor 'right-sticky-mixin)))
	       (incf (cluffer:cursor-position cursor)))))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on DELETE-ITEM-AT-POSITION.

(defmethod cluffer:delete-item-at-position ((line closed-line) position)
  (open-line line)
  (cluffer:delete-item-at-position line position))

(defmethod cluffer:delete-item-at-position ((line open-line) position)
  (let ((contents (contents line)))
    (cond ((< position (gap-start line))
	   (decf (gap-end line) (- (gap-start line) position))
	   (replace contents contents
		    :start2 position :end2 (gap-start line)
		    :start1 (gap-end line))
	   (setf (gap-start line) position))
	  ((> position (gap-start line))
	   (replace contents contents
		    :start2 (gap-end line)
		    :start1 (gap-start line) :end1 position)
	   (incf (gap-end line) (- position (gap-start line)))
	   (setf (gap-start line) position))
	  (t
	   nil))
    (setf (aref contents (gap-end line)) 0)  ; for the GC
    (incf (gap-end line))
    (when (and (> (length contents) 32)
	       (> (- (gap-end line) (gap-start line))
		  (* 3/4 (length contents))))
      (let* ((new-length (floor (length contents) 2))
	     (diff (- (length contents) new-length))
	     (new-contents (make-array new-length)))
	(replace new-contents contents
		 :start2 0 :start1 0 :end2 (gap-start line))
	(replace new-contents contents
		 :start2 (gap-end line) :start1 (- (gap-end line) diff))
	(decf (gap-end line) diff)
	(setf (contents line) new-contents)))
    (loop for cursor in (cursors line)
	  do (when (> (cluffer:cursor-position cursor) position)
	       (decf (cluffer:cursor-position cursor)))))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on END-OF-LINE.
;;;
;;; Position the cursor at the end of the line.

(defmethod cluffer:end-of-line
    ((cursor attached-cursor))
  (setf (cluffer:cursor-position cursor)
	(cluffer:item-count (cluffer:line cursor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on ITEM-AT-POSITION.

;;; No need to open the line.
(defmethod cluffer-item-at-position ((line closed-line) position)
  (aref (contents line) position))

(defmethod cluffer-item-at-position ((line open-line) position)
  (aref (contents line)
	(if (< position (gap-start line))
	    position
	    (+ position (- (gap-end line) (gap-start line))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on CLUFFER-INTERNAL:SPLIT-LINE.

(defmethod cluffer-internal:split-line ((cursor closed-cursor-mixin))
  (let* ((pos (cluffer:cursor-position cursor))
	 (line (cluffer:line cursor))
	 (contents (contents line))
	 (new-contents (subseq contents pos))
	 (new-line (make-instance 'closed-line
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

(defmethod cluffer-internal:split-line ((cursor open-cursor-mixin))
  (close-line (cluffer:line cursor))
  (cluffer-internal:split-line cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on CLUFFER-INTERNAL:JOIN-LINE.

(defmethod cluffer-internal:join-line ((line1 open-line) line2)
  (close-line line1)
  (cluffer-internal:join-line line1 line2))

(defmethod cluffer-internal:join-line (line1 (line2 open-line))
  (close-line line2)
  (cluffer-internal:join-line line1 line2))

(defmethod cluffer-internal:join-line ((line1 closed-line) (line2 closed-line))
  (loop with length = (length (contents line1))
	for cursor in (cursors line2)
	do (setf (cluffer:line cursor) line1)
	   (incf (cluffer:cursor-position cursor) length))
  (setf (contents line1)
	(concatenate 'vector (contents line1) (contents line2)))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on generic function BUFFER.
;;;
;;; Given a line or a cursor, return the buffer to which
;;; the line or cursor belongs.

(defmethod buffer ((line line))
  (cluffer:buffer (cluffer-internal:dock line)))

(defmethod buffer ((cursor attached-cursor))
  (cluffer:buffer (cluffer:line cursor)))
