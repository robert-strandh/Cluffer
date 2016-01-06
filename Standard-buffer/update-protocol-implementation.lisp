(cl:in-package #:cluffer-standard-buffer)

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

(defmethod cluffer:update ((buffer buffer) time sync skip modify create)
  (when (null time)
    (setf time -1))
  (let (;; We maintain a STATE that can be either :SKIP or :MODIFY.
	;; Initially, we are in the :SKIP state.
	(state :skip)
	;; Whenever we enter the :SKIP state, we save the current
	;; offset in FIRST-SKIP.  Whenever we leave the :SKIP state,
	;; we issue a SKIP editing operation with the difference
	;; between the current offset and FIRST-SKIP.
	(first-skip 0)
	;; We maintain the offset of nodes as we traverse the tree.
	(offset 0))
    (flet ((issue-skip ()
	     (let ((skip-count (- offset first-skip)))
	       (unless (zerop skip-count)
		 (funcall skip skip-count))))
	   (line-count (node-or-nil)
	     (if (null node-or-nil)
		 0
		 (line-count node-or-nil)))
	   (left (node)
	     (clump-binary-tree:left node))
	   (right (node)
	     (clump-binary-tree:right node)))
      (flet ((pre (node)
	       (if (eq state :skip)
		   (if (> (max-modify-time node) time)
		       ;; We are in the :SKIP state and some nodes of
		       ;; this sub-tree have been modified.  We must
		       ;; traverse the left sub-tree in case some of
		       ;; theme are located there.  Return true to
		       ;; inform ITERATIVE-TRAVERSAL that it should
		       ;; traverse the left sub-tree.
		       t
		       ;; We are in the :SKIP state and none of the
		       ;; nodes of this sub-tree have been modified.
		       (progn (incf offset (line-count (left node)))
			      ;; Return NIL to inform
			      ;; ITERATIVE-TRAVERSAL that it should
			      ;; skip the left sub-tree.
			      nil))
		   ;; We are in the modify state.  Whether any nodes
		   ;; of this sub-tree have been modified or not, we
		   ;; need to go down the left sub-tree, either to
		   ;; find more modified modes, or to find the first
		   ;; unmodified not in order to issue a SYNC
		   ;; operation.  Therefore, return true to inform
		   ;; ITERATIVE-TRAVERSAL that it should traverse the
		   ;; left sub-tree.
		   t))
	     (in (node)
	       (let ((line (cluffer-internal:line node)))
		 (if (eq state :skip)
		     ;; We are in the :SKIP state
		     (if (> (max-modify-time node) time)
			 ;; We are in the :SKIP state and some of the
			 ;; nodes in this sub-tree have been modified.
			 (if (> (modify-time node) time)
			     ;; We are in the :SKIP state and the
			     ;; current node has been modified.  We must
			     ;; issue a SKIP operation and then either a
			     ;; CREATE or a MODIFY operation according
			     ;; to whether the current node is new or
			     ;; not.
			     (progn (issue-skip)
				    (if (> (create-time node) time)
					(funcall create line)
					(funcall modify line))
				    (setf state :modify)
				    (incf offset)
				    ;; Return true to inform
				    ;; ITERATIVE-TRAVERSAL that it
				    ;; should traverse the right
				    ;; sub-tree.
				    t)
			     ;; We are in the SKIP state and this node
			     ;; has not been modified.  But some other
			     ;; nodes in this sub-tree have been
			     ;; modified.
			     (progn (incf offset)
				    ;; Return true to inform
				    ;; ITERATIVE-TRAVERSAL that it
				    ;; should traverse the right
				    ;; sub-tree.
				    t))
			 ;; We are in the :SKIP state and none nodes in
			 ;; this sub-tree have been modified.
			 (progn (incf offset (1+ (line-count (right node))))
				;; Return false to inform
				;; ITERATIVE-TRAVERSAL that it should
				;; not traverse the right sub-tree.
				nil))
		     ;; We are in the MODIFY state
		     (if (> (modify-time node) time)
			 ;; We are in the :MODIFY state and this node
			 ;; has been modified.  We must issue either a
			 ;; MODIFY or a CREATE operation.
			 (progn (if (> (create-time node) time)
				    (funcall create line)
				    (funcall modify line))
				(incf offset)
				;; Return true to inform
				;; ITERATIVE-TRAVERSAL that it should
				;; traverse the right sub-tree.
				t)
			 ;; We are in the :MODIFY state and this node
			 ;; has NOT been modified.  We issue a SYNC
			 ;; operations and set the state to :SKIP
			 (progn (funcall sync line)
				(incf offset)
				(setf state :SKIP)
				;; Return true to inform
				;; ITERATIVE-TRAVERSAL that it should
				;; traverse the right sub-tree.
				t))))))
	(clump-binary-tree:iterative-traversal
	 (contents buffer) #'pre #'in #'identity)
	;; Now, if we are in the :SKIP state at the end of the buffer,
	;; we issue a skip with the number of remaining nodes to skip.
	(issue-skip))))
  (current-time buffer))
