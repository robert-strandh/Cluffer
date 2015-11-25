(cl:in-package #:cluffer-buffer)

;;;; This function implements the buffer update protocol for a buffer
;;;; represented as a splay tree.
;;;;
;;;; The purpose of the buffer update protocol is to allow for a
;;;; number of edit operations to the buffer without updating the view
;;;; of the buffer.  This functionality is important because a single
;;;; command may result in an arbitrary number of edit operations to
;;;; the buffer, and we typically want the view to be updated only
;;;; once, when all those edit operations have been executed.
;;;;
;;;; The buffer update protocol has been designed to allow for
;;;; different representations of the view.  Seen from the UPDATE
;;;; function, the view takes the form of four different editing
;;;; operations, namely CREATE, MODIFY, SYNC, and SKIP.  These editing
;;;; operations are represented as functions that are supplied by
;;;; client code as arguments to the UPDATE function.  The editing
;;;; operations only assume that the view keeps a copy of the
;;;; structure of the lines of the buffer, and that this copy has a
;;;; cursor that is affected by the editing operations.  This cursor
;;;; can be before the first line of the view, after the last line of
;;;; the view, or between two lines of the view.  When BUFFER is
;;;; called by client code, the cursor is located before the first
;;;; line of the view.
;;;;
;;;; BUFFER is a buffer that might have been modified since the last
;;;; call to UPDATE.  TIME is the last time UPDATE was called; the
;;;; UPDATE function will report modifications since that time.
;;;;
;;;; The editing operations have the following meaning:
;;;;
;;;;  * CREATE indicates that a line has been created.  The function
;;;;    CREATE is called with the line as the argument.  Client code
;;;;    should insert the new line at the position of the cursor, and
;;;;    then leave the cursor positioned immediately after the
;;;;    inserted line.
;;;;
;;;;  * MODIFY indicates that a line has been modified.  The function
;;;;    MODIFY is called with the modified line as the argument.  The
;;;;    line that has been modified is the one immediately after the
;;;;    cursor.  At the end of this operation, client code should
;;;;    leave the cursor positioned immediately after the modified
;;;;    line.
;;;;
;;;;  * SYNC indicates the first unmodified line after a sequence of
;;;;    new or modified lines.  Accordingly, this function is called
;;;;    once, following one or more calls to CREATE or MODIFY.  This
;;;;    function is called with a single argument: the unmodified
;;;;    line.  Call this line L.  Client code must delete lines
;;;;    immediately following the cursor until the line immediately
;;;;    following the cursor is L.  At the end of this operation,
;;;;    client code should leave the cursor positioned immediately
;;;;    after L.
;;;;
;;;;  * SKIP indicates that a number of lines have not been subject to
;;;;    any modifications since the last call to UPDATE.  The SKIP
;;;;    function takes a single argument: the number of lines to skip.
;;;;    Call this number N.  The SKIP function is called first, to
;;;;    indicate that a prefix of length N of the buffer is
;;;;    unmodified, or after a SYNC operation to indicate that N lines
;;;;    following the one given as argument to the SYNC operation are
;;;;    unmodified.  Client code should move the cursor forward N
;;;;    positions.

(defun update (buffer time sync skip modify create)
  nil)
