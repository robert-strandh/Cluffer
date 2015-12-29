(cl:in-package #:cluffer-base)

;;; Default method on ITEM-COUNT specialized to CURSOR.
;;;
;;; This method calls the generic function LINE with the cursor as
;;; argument order to obtain the line to which the cursor is attached.
;;; It then calls ITEM-COUNT on that line.
(defmethod cluffer:item-count ((cursor cluffer:cursor))
  (cluffer:item-count (cluffer:line cursor)))

;;; Default method on BEGINNING-OF-LINE-P.
;;;
;;; Simple implementations of the line protocol can rely on the
;;; existence of this method.  More sophisticated implementations
;;; might use a more optimized way of determining whether the cursor
;;; is at the beginning of the line.
(defmethod cluffer:beginning-of-line-p ((cursor cluffer:cursor))
  (zerop (cluffer:cursor-position cursor)))

;;; Default method on END-OF-LINE-P.
;;;
;;; Simple implementations of the line protocol can rely on the
;;; existence of this method.  More sophisticated implementations
;;; might use a more optimized way of determining whether the cursor
;;; is at the end of the line.
(defmethod cluffer:end-of-line-p ((cursor cluffer:cursor))
  (= (cluffer:cursor-position cursor)
     (cluffer:item-count cursor)))

;;; Default method on BEGINNING-OF-LINE.
;;;
;;; Simple implementations of the line protocol can rely on the
;;; existence of this method.  More sophisticated implementations
;;; might use a more optimized way of positioning the cursor at the
;;; end of the line.

(defmethod cluffer:beginning-of-line ((cursor cluffer:cursor))
  (setf (cluffer:cursor-position cursor) 0))

;;; Default method on FIRST-LINE-P.
(defmethod cluffer:first-line-p ((line cluffer:line))
  (= (cluffer:line-number line) 0))

;;; Default method on LAST-LINE-P.
(defmethod cluffer:last-line-p ((line cluffer:line))
  (= (cluffer:line-number line)
     (1- (cluffer:line-count (cluffer:buffer line)))))

;;; Default method on BEGINNING-OF-BUFFER-P.
(defmethod cluffer:beginning-of-buffer-p ((cursor cluffer:cursor))
  (and (cluffer:beginning-of-line-p cursor)
       (cluffer:first-line-p (cluffer:line cursor))))

;;; Default method on END-OF-BUFFER-P.
(defmethod cluffer:end-of-buffer-p ((cursor cluffer:cursor))
  (and (cluffer:end-of-line-p cursor)
       (cluffer:last-line-p (cluffer:line cursor))))

;;; Default method on FORWARD-ITEM.
(defmethod cluffer:forward-item ((cursor cluffer:cursor))
  (incf (cluffer:cursor-position cursor)))

;;; Default method on BACKWARD-ITEM.
(defmethod cluffer:backward-item ((cursor cluffer:cursor))
  (decf (cluffer:cursor-position cursor)))

;;; Default method on ITEMS specialized to CURSOR
;;;
;;; A :BEFORE method has already checked that the cursor is attached,
;;; so there is no need to do that again.
(defmethod cluffer:items ((cursor cluffer:cursor) &key (start 0) end)
  (cluffer:items (cluffer:line cursor) :start start :end end))

;;; Default method on ITEM-BEFORE-CURSOR.
;;;
;;; A :BEFORE method has already checked that the cursor is attached,
;;; so there is no need to do that again.
;;;
;;; A :BEFORE method on ITEM-AT-POSITION will check that (1- POSITION)
;;; is a valid item position.
(defmethod cluffer:item-before-cursor ((cursor cluffer:cursor))
  (let ((line (cluffer:line cursor))
	(position (cluffer:cursor-position cursor)))
    (cluffer:item-at-position line (1- position))))

;;; Default method on ITEM-AFTER-CURSOR.
;;;
;;; A :BEFORE method has already checked that the cursor is attached,
;;; so there is no need to do that again.
;;;
;;; A :BEFORE method on ITEM-AT-POSITION will check that POSITION is a
;;; valid item position.
(defmethod cluffer:item-after-cursor ((cursor cluffer:cursor))
  (let ((line (cluffer:line cursor))
	(position (cluffer:cursor-position cursor)))
    (cluffer:item-at-position line position)))

;;; Default method on INSERT-ITEM.
;;;
;;; A :BEFORE method has already checked that the cursor is attached,
;;; so there is no need to do that again.
;;;
;;; A :BEFORE method on INSERT-ITEM-AT-POSITION will check that
;;; POSITION is a valid insertion position, although for this
;;; function, that check is redundant since every possible cursor
;;; position is a valid inserition position.
(defmethod cluffer:insert-item ((cursor cluffer:cursor) item)
  (let ((line (cluffer:line cursor))
	(position (cluffer:cursor-position cursor)))
    (cluffer:insert-item-at-position line item position)))

;;; Default method on DELETE-ITEM.
;;;
;;; A :BEFORE method has already checked that the cursor is attached,
;;; so there is no need to do that again.
;;;
;;; A :BEFORE method on DELETE-ITEM-AT-POSITION will check that
;;; POSITION is a valid position for deletion.
(defmethod cluffer:delete-item ((cursor cluffer:cursor))
  (let ((line (cluffer:line cursor))
	(position (cluffer:cursor-position cursor)))
    (cluffer:delete-item-at-position line position)))

;;; Default method on ERASE-ITEM.
;;;
;;; A :BEFORE method has already checked that the cursor is attached,
;;; so there is no need to do that again.
;;;
;;; A :BEFORE method on DELETE-ITEM-AT-POSITION will check that
;;; POSITION is a valid position for deletion.
(defmethod cluffer:erase-item ((cursor cluffer:cursor))
  (let ((line (cluffer:line cursor))
	(position (cluffer:cursor-position cursor)))
    (cluffer:delete-item-at-position line (1- position))))

;;; Default method on NOTIFY-ITEM-COUNT-CHANGED.  This method does
;;; nothing, and is used when a line is not attached to a buffer.
(defmethod cluffer-internal:notify-item-count-changed ((doc null) delta)
  nil)

;;; Default method on SPLIT-LINE.  This method calls
;;; SLIT-LINE-AT-POSITION with the line of the cursor and the position
;;; of the cursor as arguments.
(defmethod cluffer:split-line ((cursor cluffer:cursor))
  (let ((line (cluffer:line cursor))
	(position (cluffer:cursor-position cursor)))
    (cluffer:split-line-at-position line position)))

;;; Default method on SPLIT-LINE-AT-POSITION.  This method calls the
;;; internal generic function DOCK-SPLIT-LINE with the dock of the
;;; line, the line, and the position.
(defmethod cluffer:split-line-at-position ((line cluffer:line) position)
  (cluffer-internal:dock-split-line
   (cluffer-internal:dock line) line position))

;;; Default method on LINE-NUMBER.  This method calls the internal
;;; generic function DOCK-LINE-NUMBER with the dock of the line and
;;; the line.
(defmethod cluffer:line-number ((line cluffer:line))
  (cluffer-internal:dock-line-number (cluffer-internal:dock line) line))

;;; Default method on ITEM-AT-POSITION, invoked when the LINE argument
;;; is not an instance of LINE.
(defmethod cluffer:item-at-position (line position)
  (error 'cluffer:object-must-be-line
	 :object line))

;;; Default method on INSERT-ITEM-AT-POSITION, invoked when the LINE
;;; argument is not an instance of LINE.
(defmethod cluffer:insert-item-at-position (line item position)
  (declare (ignore item position))
  (error 'cluffer:object-must-be-line
	 :object line))

;;; Default method on DELETE-ITEM-AT-POSITION, invoked when the LINE
;;; argument is not an instance of LINE.
(defmethod cluffer:delete-item-at-position (line position)
  (declare (ignore position))
  (error 'cluffer:object-must-be-line
	 :object line))

;;; Default method on LINE-COUNT, invoked when the BUFFER argument is
;;; not an instance of BUFFER.
(defmethod cluffer:line-count (buffer)
  (error 'cluffer:object-must-be-buffer
	 :object buffer))

;;; Default method on FIND-LINE, invoked when the BUFFER argument is
;;; not an instance of BUFFER.
(defmethod cluffer:find-line (buffer line-number)
  (declare (ignore line-number))
  (error 'cluffer:object-must-be-buffer
	 :object buffer))

;;; Default method on BUFFER specialized to CURSOR.
(defmethod cluffer:buffer ((entity cluffer:cursor))
  (cluffer:buffer (cluffer:line entity)))

;;; Default method on BUFFER specialized to LINE.
(defmethod cluffer:buffer ((entity cluffer:line))
  (cluffer-internal:buffer (cluffer-internal:dock entity)))
