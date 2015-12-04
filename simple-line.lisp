(cl:in-package #:cluffer-simple-line)

;;;; This file contains a very simple and very inefficient
;;;; implementation of the line protocol.  It is included here for two
;;;; reasons.  First, we use it to compare results of random
;;;; operations to the same operations on the standard line
;;;; implementation.  Second, it illustrates how to implement the line
;;;; protocol in a simple way, so that clients can use it as a
;;;; starting point for their own implementations of the protocol.

(defclass line (cluffer-buffer:line)
   ((%contents :initarg :contents :accessor contents)
    (%cursors :initarg :cursors :accessor cursors)))

(defclass left-sticky-cursor
    (cluffer-buffer:attached-cursor
     cursor-mixin
     cluffer-buffer:left-sticky-mixin)
  ())

(defclass right-sticky-cursor
    (cluffer-buffer:attached-cursor
     cursor-mixin
     cluffer-buffer:right-sticky-mixin)
  ())

(defmethod cluffer-buffer:item-count ((line line))
  (length (contents line)))

(defun make-empty-line ()
  (make-instance 'line
    :cursors '()
    :contents (make-array 0 :fill-pointer t)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on ITEMS.

;;; When all the items are asked for, we do not allocate a fresh
;;; vector.  This means that client code is not allowed to mutate the
;;; return value of this function
(defmethod cluffer-buffer:items ((line line) &key (start 0) (end nil))
  (if (and (= start 0) (null end))
      (contents line)
      (subseq (contents line) start end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Detaching and attaching a cursor.

(defmethod cluffer-buffer:attach-cursor
    ((cursor cluffer-buffer:attached-cursor) line &optional position)
  (declare (ignore line position))
  (error 'cluffer-buffer:cursor-attached))

(defmethod cluffer-buffer:attach-cursor
    ((cursor cluffer-buffer:detached-left-sticky-cursor)
     (line line)
     &optional
       (position 0))
  (when (> position (cluffer-buffer:item-count line))
    (error 'cluffer-buffer:end-of-line))
  (push cursor (cursors line))
  (change-class cursor 'left-sticky-cursor
		:line line
		:cursor-position position)
  nil)

(defmethod cluffer-buffer:attach-cursor
    ((cursor cluffer-buffer:detached-right-sticky-cursor)
     (line line)
     &optional
       (position 0))
  (when (> position (cluffer-buffer:item-count line))
    (error 'cluffer-buffer:end-of-line))
  (push cursor (cursors line))
  (change-class cursor 'right-sticky-cursor
		:line line
		:cursor-position position)
  nil)

(defmethod cluffer-buffer:detach-cursor
    ((cursor cluffer-buffer:detached-cursor))
  (error 'cluffer-buffer:cursor-detached))

(defmethod cluffer-buffer:detach-cursor
  ((cursor cluffer-buffer:left-sticky-mixin))
  (setf (cursors (cluffer-buffer:line cursor))
	(remove cursor (cursors (cluffer-buffer:line cursor))))
  (change-class cursor 'cluffer-buffer:detached-left-sticky-cursor)
  nil)

(defmethod cluffer-buffer:detach-cursor
  ((cursor cluffer-buffer:right-sticky-mixin))
  (setf (cursors (cluffer-buffer:line cursor))
	(remove cursor (cursors (cluffer-buffer:line cursor))))
  (change-class cursor 'cluffer-buffer:detached-right-sticky-cursor)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Operations on cursors.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on BEGINNING-OF-LINE-P.
;;;
;;; Given a cursor, return true if and only if it is at the beginning
;;; of the line.

(defmethod cluffer-buffer:beginning-of-line-p
    ((cursor cluffer-buffer:detached-cursor))
  (error 'cluffer-buffer:cursor-detached))

;;; The default method just calls CURSOR-POSITION and returns true if
;;; and only if that position is 0.
(defmethod cluffer-buffer:beginning-of-line-p
    ((cursor cluffer-buffer:attached-cursor))
  (zerop (cluffer-buffer:cursor-position cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on END-OF-LINE-P.

(defmethod cluffer-buffer:end-of-line-p
    ((cursor cluffer-buffer:detached-cursor))
  (error 'cluffer-buffer:cursor-detached))

;;; The default method just calls CURSOR-POSITION and returns true if
;;; and only if that position is the same as the number of items in
;;; the line.
(defmethod cluffer-buffer:end-of-line-p
    ((cursor cluffer-buffer:attached-cursor))
  (= (cluffer-buffer:cursor-position cursor)
     (cluffer-buffer:item-count (cluffer-buffer:line cursor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on INSERT-ITEM.

(defmethod cluffer-buffer:insert-item ((cursor cursor-mixin) item)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on DELETE-ITEM.

(defmethod cluffer-buffer:delete-item ((cursor cursor-mixin))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on ERASE-ITEM.

(defmethod cluffer-buffer:erase-item ((cursor cursor-mixin))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on FORWARD-ITEM

(defmethod cluffer-buffer:forward-item ((cursor cursor-mixin))
  (when (cluffer-buffer:end-of-line-p cursor)
    (error 'cluffer-buffer:end-of-line))
  (incf (cluffer-buffer:cursor-position cursor))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on BACKWARD-ITEM

(defmethod cluffer-buffer:backward-item ((cursor cursor-mixin))
  (when (cluffer-buffer:beginning-of-line-p cursor)
    (error 'cluffer-buffer:beginning-of-line))
  (decf (cluffer-buffer:cursor-position cursor))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on BEGINNING-OF-LINE.
;;;
;;; Position the cursor at the beginning of the line.

(defmethod cluffer-buffer:beginning-of-line
    ((cursor cluffer-buffer:detached-cursor))
  (error 'cluffer-buffer:cursor-detached))

(defmethod cluffer-buffer:beginning-of-line
    ((cursor cluffer-buffer:attached-cursor))
  (setf (cluffer-buffer:cursor-position cursor) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on END-OF-LINE.
;;;
;;; Position the cursor at the end of the line.

(defmethod cluffer-buffer:end-of-line
    ((cursor cluffer-buffer:detached-cursor))
  (error 'cluffer-buffer:cursor-detached))

(defmethod cluffer-buffer:end-of-line
    ((cursor cluffer-buffer:attached-cursor))
  (setf (cluffer-buffer:cursor-position cursor)
	(cluffer-buffer:item-count (cluffer-buffer:line cursor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on ITEM-BEFORE-CURSOR.

(defmethod cluffer-buffer:item-before-cursor
    ((cursor cursor-mixin))
  (when (cluffer-buffer:beginning-of-line-p cursor)
    (error 'cluffer-buffer:beginning-of-line))
  (aref (contents (cluffer-buffer:line cursor))
	(1- (cluffer-buffer:cursor-position cursor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on ITEM-AFTER-CURSOR.

(defmethod cluffer-buffer:item-after-cursor
    ((cursor cursor-mixin))
  (when (cluffer-buffer:beginning-of-line-p cursor)
    (error 'cluffer-buffer:beginning-of-line))
  (aref (contents (cluffer-buffer:line cursor))
	(cluffer-buffer:cursor-position cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on LINE-SPLIT-LINE.

(defmethod cluffer-buffer:line-split-line ((cursor cursor-mixin))
  (let* ((pos (cluffer-buffer:cursor-position cursor))
	 (line (cluffer-buffer:line cursor))
	 (contents (contents line))
	 (new-contents (subseq contents pos))
	 (new-line (make-instance 'line
		     :cursors '()
		     :contents new-contents)))
    (setf (contents line)
	  (subseq (contents line) 0 pos))
    (setf (cursors new-line)
	  (loop for cursor in (cursors line)
		when (or (and (typep cursor 'cluffer-buffer:right-sticky-mixin)
			      (>= (cluffer-buffer:cursor-position cursor) pos))
			 (and (typep cursor 'cluffer-buffer:left-sticky-mixin)
			      (> (cluffer-buffer:cursor-position cursor) pos)))
		  collect cursor))
    (loop for cursor in (cursors new-line)
	  do (setf (cluffer-buffer:line cursor) new-line)
	     (decf (cluffer-buffer:cursor-position cursor) pos))
    (setf (cursors line)
	  (set-difference (cursors line) (cursors new-line)))
    new-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on LINE-JOIN-LINE.

(defmethod cluffer-buffer:line-join-line ((line1 line) (line2 line))
  (loop with length = (length (contents line1))
	for cursor in (cursors line2)
	do (setf (cluffer-buffer:line cursor) line1)
	   (incf (cluffer-buffer:cursor-position cursor) length))
  (setf (contents line1)
	(concatenate 'vector (contents line1) (contents line2)))
  nil)
