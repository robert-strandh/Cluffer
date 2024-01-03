(cl:in-package #:cluffer-base)

(defclass cluffer:buffer () ())

(defclass cluffer:line ()
  ((%dock :initarg :dock :initform nil
          :accessor cluffer-internal:dock)))

(defclass cluffer:cursor () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function LINE-COUNT.
;;;
;;; Return the number of lines in BUFFER.

(defgeneric cluffer:line-count (buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ITEM-COUNT.
;;;
;;; Return the number of items in ENTITY.
;;;
;;; If ENTITY is a buffer, then the total number of items in the
;;; buffer is returned.
;;;
;;; IF ENTITY is a line, then the number of items in the line is
;;; returned.
;;;
;;; If ENTITY is a cursor that is currently attached to a line, the
;;; number of items of the line to which the cursor is attached is
;;; returned.
;;;
;;; If ENTITY is a cursor that is currently not attached to a line,
;;; then an error of type DETACHED-CURSOR is signaled.

(defgeneric cluffer:item-count (entity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function CURSOR-ATTACHED-P.
;;;
;;; Given a cursor, return true if and only if the cursor is attached
;;; to a line.

(defgeneric cluffer:cursor-attached-p (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function CURSOR-POSITION.
;;;
;;; Given a cursor, return its conceptual position.
;;;
;;; If CURSOR is currently not attached to a line, then an error of
;;; type DETACHED-CURSOR is signaled.

(defgeneric cluffer:cursor-position (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF CURSOR-POSITION).
;;;
;;; Given a cursor, set its conceptual position.
;;;
;;; If POSITION is negative or greater than the item count of the line
;;; to which the cursor is attached, then an error is signaled.
;;;
;;; If CURSOR is currently not attached to a line, then an error of
;;; type DETACHED-CURSOR is signaled.

(defgeneric (setf cluffer:cursor-position) (position cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function BEGINNING-OF-LINE-P.
;;;
;;; Return true if and only if CURSOR is positioned at the beginning
;;; of the line to which it is attached.
;;;
;;; If CURSOR is currently not attached to a line, then an error of
;;; type DETACHED-CURSOR is signaled.
;;;
;;; Calling this function has the same effect as calling the function
;;; CURSOR-POSITION and checking whether the return value is zero, but
;;; this function might be implemented differently.

(defgeneric cluffer:beginning-of-line-p (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function END-OF-LINE-P.
;;;
;;; Return true if and only if CURSOR is positioned at the end of the
;;; line to which it is attached.
;;;
;;; If CURSOR is currently not attached to a line, then an error of
;;; type DETACHED-CURSOR is signaled.
;;;
;;; Calling this function has the same effect as calling the functions
;;; CURSOR-POSITION and ITEM-COUNT checking whether the return values
;;; are the same, but this function might be implemented differently.

(defgeneric cluffer:end-of-line-p (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FORWARD-ITEM.
;;;
;;; CURSOR is moved forward by one position.
;;;
;;; If CURSOR is already at the end of the line to which it is
;;; attached, then the error END-OF-LINE is signaled.
;;;
;;; If CURSOR is currently not attached to a line, then an error of
;;; type DETACHED-CURSOR is signaled.

(defgeneric cluffer:forward-item (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function BACKWARD-ITEM.
;;;
;;; CURSOR is moved backward by one position.
;;;
;;; If CURSOR is already at the beginning of the line to which it is
;;; attached, then the error BEGINNING-OF-LINE is signaled.
;;;
;;; If CURSOR is currently not attached to a line, then an error of
;;; type DETACHED-CURSOR is signaled.

(defgeneric cluffer:backward-item (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function BEGINNING-OF-LINE.
;;;
;;; CURSOR is moved so that it is positioned at the beginning of the
;;; line to which it is attached.
;;;
;;; If CURSOR is currently not attached to a line, then an error of
;;; type DETACHED-CURSOR is signaled.

(defgeneric cluffer:beginning-of-line (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function END-OF-LINE.
;;;
;;; CURSOR is moved so that it is positioned at the end of the line to
;;; which it is attached.
;;;
;;; If CURSOR is currently not attached to a line, then an error of
;;; type DETACHED-CURSOR is signaled.

(defgeneric cluffer:end-of-line (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ITEM-AT-POSITION.
;;;
;;; Return the item at POSITION in LINE.

(defgeneric cluffer:item-at-position (line position))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ITEM-BEFORE-CURSOR.
;;;
;;; Return the item that occurs immediately before CURSOR.
;;;
;;; If CURSOR is at the beginning of the line to which it is attached,
;;; then the error BEGINNING-OF-LINE is signaled.
;;;
;;; If CURSOR is currently not attached to a line, then an error of
;;; type DETACHED-CURSOR is signaled.

(defgeneric cluffer:item-before-cursor (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ITEM-AFTER-CURSOR.
;;;
;;; Return the item that occurs immediately after CURSOR.
;;;
;;; If CURSOR is at the end of the line to which it is attached, then
;;; the error END-OF-LINE is signaled.
;;;
;;; If CURSOR is currently not attached to a line, then an error of
;;; type DETACHED-CURSOR is signaled.

(defgeneric cluffer:item-after-cursor (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function INSERT-ITEM-AT-POSITION.
;;;
;;; Insert ITEM into LINE at POSITION.
;;;
;;; How the positions of cursors located at POSITION are affected by
;;; this operation is defined by the exact type of the line and the
;;; exact types of these cursors.
;;;
;;; The STANDARD-LINE implementation supplies two different cursor
;;; types, namely LEFT-STICKY and RIGHT-STICKY cursors.  each
;;; LEFT-STICKY cursor at POSITION will maintain its position, whereas
;;; each RIGHT-STICKY cursor at POSITION will have its position
;;; incremented as a result of this operation.

(defgeneric cluffer:insert-item-at-position (line item position))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function DELETE-ITEM-AT-POSITION.
;;;
;;; Delete the item at POSITION in LINE.

(defgeneric cluffer:delete-item-at-position (line position))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function INSERT-ITEM.
;;;
;;; Insert ITEM into the line to which CURSOR is attached at the
;;; current position of CURSOR.
;;;
;;; How the position of CURSOR (and other cursors at the same position
;;; as CURSOR) is affected by this operation is defined by the exact
;;; types of these cursors.
;;;
;;; If CURSOR is currently not attached to a line, then an error of
;;; type DETACHED-CURSOR is signaled.

(defgeneric cluffer:insert-item (cursor item))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function DELETE-ITEM.
;;;
;;; Delete the item immediately after CURSOR.
;;;
;;; If CURSOR is at the end of the line to which it is attached, then
;;; the error END-OF-LINE is signaled.
;;;
;;; If CURSOR is currently not attached to a line, then an error of
;;; type DETACHED-CURSOR is signaled.

(defgeneric cluffer:delete-item (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ERASE-ITEM.
;;;
;;; Delete the item immediately before CURSOR.
;;;
;;; If CURSOR is at the beginning of the line to which it is attached,
;;; then the error BEGINNING-OF-LINE is signaled.
;;;
;;; If CURSOR is currently not attached to a line, then an error of
;;; type DETACHED-CURSOR is signaled.

(defgeneric cluffer:erase-item (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function LINE-NUMBER.
;;;
;;; Return the line number of ENTITY.
;;;
;;; If ENTITY is a line, then the number of that line in the buffer to
;;; which the line belongs is returned.  The first line in the buffer
;;; has number 0.
;;;
;;; If ENTITY is a cursor, then the number of the line to which the
;;; cursor is attached is returned.
;;;
;;; If ENTITY is a cursor that is currently not attached to a line,
;;; then an error of type DETACHED-CURSOR is signaled.

(defgeneric cluffer:line-number (entity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FIND-LINE.
;;;
;;; Return the line with the number LINE-NUMBER in BUFFER.
;;;
;;; The first line in the buffer has number 0.
;;;
;;; If LINE-NUMBER is negative, then an error of type
;;; BEGINNING-OF-BUFFER is signaled.  If LINE-NUMBER is greater than
;;; or equal to the number of lines in BUFFER, then an error of type
;;; END-OF-BUFFER is signaled.

(defgeneric cluffer:find-line (buffer line-number))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function BEGINNING-OF-BUFFER-P.
;;;
;;; Return true if and only if CURSOR is positioned at the beginning
;;; of the buffer.
;;;
;;; If CURSOR is currently not attached to a line, then an error of
;;; type DETACHED-CURSOR is signaled.
;;;
;;; Calling this function has the same effect as calling the functions
;;; LINE-NUMBER and BEGINNING-OF-LINE-P, checking that the line number
;;; of the cursor is 0 and that BEGINNING-OF-LINE-P returns true, but
;;; the actual implementation of this function might be different for
;;; performance reasons.

(defgeneric cluffer:beginning-of-buffer-p (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function END-OF-BUFFER-P.
;;;
;;; Return true if and only if CURSOR is positioned at the end
;;; of the buffer.
;;;
;;; If CURSOR is currently not attached to a line, then an error of
;;; type DETACHED-CURSOR is signaled.
;;;
;;; Calling this function has the same effect as calling the functions
;;; LINE-COUNT, LINE-NUMBER, and END-OF-LINE-P, checking that the line
;;; number of the cursor is the last line of the buffer and that
;;; END-OF-LINE-P returns true, but the actual implementation of this
;;; function might be different for performance reasons.

(defgeneric cluffer:end-of-buffer-p (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SPLIT-LINE-AT-POSITION.

(defgeneric cluffer:split-line-at-position (line position))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SPLIT-LINE.

(defgeneric cluffer:split-line (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function JOIN-LINE.
;;;
;;; If entity is a line, then the line following it is appended to
;;; the end of this line.  If entity is a cursor, then the line to
;;; which the cursor is attached is joined with the line following it.

(defgeneric cluffer:join-line (entity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ITEMS.
;;;
;;; Return the items of ENTITY in a vector.  If ENTITY is a line, then
;;; return the items in that line.  If ENTITY is a cursor, then return
;;; the items in the line to which the cursor is attached.
;;;
;;; If entity is a cursor that is not currently attached to a line,
;;; then signal a condition of type CURSOR-DETACHED.

(defgeneric cluffer:items (entity &key start end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ATTACH-CURSOR.

(defgeneric cluffer:attach-cursor (cursor line &optional position))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function DETACH-CURSOR.

(defgeneric cluffer:detach-cursor (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function BUFFER.
;;;
;;; Return the buffer of a cursor or a line.

(defgeneric cluffer:buffer (entity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FIRST-LINE-P.

(defgeneric cluffer:first-line-p (line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function LAST-LINE-P.

(defgeneric cluffer:last-line-p (line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function LINE
;;;
;;; Return the line of CURSOR.
;;;
;;; If CURSOR is a cursor that is currently not attached to a line,
;;; then an error of type DETACHED-CURSOR is signaled.

(defgeneric cluffer:line (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function CURSOR=/2
;;;
;;; Return true if CURSOR1 and CURSOR2 are positioned at the same line
;;; and the same item index in a given buffer.
;;;
;;; If CURSOR1 or CURSOR2 is not currently attached to a line, then an
;;; error of type DETACHED-CURSOR is signaled.
;;;
;;; If CURSOR1 and CURSOR2 are currently attached to lines that belong
;;; to different buffers, an error of type CURSORS-ARE-NOT-COMPARABLE
;;; is signaled.

(defgeneric cluffer:cursor=/2 (cursor1 cursor2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function CURSOR</2
;;;
;;; Return true if CURSOR1 is positioned before CURSOR2 in a given
;;; buffer.
;;;
;;; If CURSOR1 or CURSOR2 is not currently attached to a line, then an
;;; error of type DETACHED-CURSOR is signaled.
;;;
;;; If CURSOR1 and CURSOR2 are currently attached to lines that belong
;;; to different buffers, an error of type CURSORS-ARE-NOT-COMPARABLE
;;; is signaled.

(defgeneric cluffer:cursor</2 (cursor1 cursor2))

(macrolet
    ((define (name binary-predicate &key negatep)
       (let ((reducer (if negatep 'notany 'every)))
         `(progn
            (defun ,name (cursor &rest more-cursors)
              (or (null more-cursors)
                  (,reducer (function ,binary-predicate)
                            (list* cursor more-cursors) more-cursors)))

            (define-compiler-macro ,name (cursor &rest more-cursors)
              (if (null more-cursors)
                  t
                  (destructuring-bind (second-cursor &rest rest-cursors)
                      more-cursors
                    (let* ((binary `(,',binary-predicate ,cursor ,second-cursor))
                           (pair   ,(if negatep
                                        '`(not ,binary)
                                        'binary)))
                      (if (null rest-cursors)
                          pair
                          `(and ,pair (,',name ,@more-cursors)))))))))))
  (define cluffer:cursor<  cluffer:cursor</2)
  (define cluffer:cursor<= cluffer:cursor<=/2)
  (define cluffer:cursor=  cluffer:cursor=/2)
  (define cluffer:cursor>= cluffer:cursor</2  :negatep t)
  (define cluffer:cursor>  cluffer:cursor<=/2 :negatep t))

(defun cluffer:cursor/= (cursor &rest more-cursors)
  (cond ((null more-cursors)
         t)
        ((null (cdr more-cursors))
         (not (cluffer:cursor=/2 cursor (first more-cursors))))
        (t
         (loop named outer
               for cursors on (list* cursor more-cursors)
               for (cursor1 . rest) = cursors
               while rest
               do (loop for cursor2 in rest
                        when (cluffer:cursor=/2 cursor1 cursor2)
                          do (return-from outer nil))
               finally (return-from outer t)))))
