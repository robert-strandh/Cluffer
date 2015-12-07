(cl:in-package #:cluffer)

(defclass buffer () ())

(defclass line ()
  ((%dock :initarg :dock :initform nil :accessor dock)))

(defclass cursor () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ITEM-COUNT.
;;;
;;; Return the number of items in ENTITY.

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

(defgeneric item-count (entity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function CURSOR-POSITION.
;;;
;;; Given a cursor, return its conceptual position.
;;;
;;; If CURSOR is currently not attached to a line, then an error of
;;; type DETACHED-CURSOR is signaled.

(defgeneric cursor-position (cursor))

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

(defgeneric (setf cursor-position) (position cursor))

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

(defgeneric beginning-of-line-p (cursor))

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

(defgeneric end-of-line-p (cursor))

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

(defgeneric forward-item (cursor))

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

(defgeneric backward-item (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function BEGINNING-OF-LINE.
;;;
;;; CURSOR is moved so that it is positioned at the beginning of the
;;; line to which it is attached.
;;;
;;; If CURSOR is currently not attached to a line, then an error of
;;; type DETACHED-CURSOR is signaled.

(defgeneric beginning-of-line (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function END-OF-LINE.
;;;
;;; CURSOR is moved so that it is positioned at the end of the line to
;;; which it is attached.
;;;
;;; If CURSOR is currently not attached to a line, then an error of
;;; type DETACHED-CURSOR is signaled.

(defgeneric end-of-line (cursor))

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

(defgeneric item-before-cursor (cursor))

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

(defgeneric item-after-cursor (cursor))

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

(defgeneric insert-item (cursor item))

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

(defgeneric delete-item (cursor))

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

(defgeneric erase-item (cursor))

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

(defgeneric line-number (entity))

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

(defgeneric find-line (buffer line-number))
