(cl:in-package #:cluffer-standard-buffer)

(defmethod (setf clump-binary-tree:left) :before ((new-left null) (node node))
  (unless (null (clump-binary-tree:left node))
    (decf (line-count node) (line-count (clump-binary-tree:left node)))
    (decf (item-count node) (item-count (clump-binary-tree:left node)))
    (setf (max-modify-time node)
	  (max (modify-time node)
	       (if (null (clump-binary-tree:right node))
		   0
		   (max-modify-time (clump-binary-tree:right node)))))))

(defmethod (setf clump-binary-tree:left) :after ((new-left node) (node node))
  (incf (line-count node) (line-count new-left))
  (incf (item-count node) (item-count new-left))
  (setf (max-modify-time node)
	(max (max-modify-time node)
	     (max-modify-time new-left))))

(defmethod (setf clump-binary-tree:right) :before ((new-right null) (node node))
  (unless (null (clump-binary-tree:right node))
    (decf (line-count node) (line-count (clump-binary-tree:right node)))
    (decf (item-count node) (item-count (clump-binary-tree:right node)))
    (setf (max-modify-time node)
	  (max (modify-time node)
	       (if (null (clump-binary-tree:left node))
		   0
		   (max-modify-time (clump-binary-tree:left node)))))))

(defmethod (setf clump-binary-tree:right) :after ((new-right node) (node node))
  (incf (line-count node) (line-count new-right))
  (incf (item-count node) (item-count new-right))
  (setf (max-modify-time node)
	(max (max-modify-time node)
	     (max-modify-time new-right))))

(defmethod clump-binary-tree:splay :after ((node node))
  (setf (contents (buffer node)) node))

;;; Make an empty buffer.  Client code can decide what line
;;; implementation to use by passing in instance of the desired class
;;; as the initial line for the buffer.
(defun make-empty-buffer (initial-line)
  (let* ((node (make-instance 'node
		     :line-count 1
		     :item-count 0
		     :create-time 0
		     :modify-time 0
		     :max-modify-time 0
		     :line initial-line))
	 (buffer (make-instance 'buffer
		   :current-time 1
		   :contents node)))
    (setf (cluffer-internal:dock initial-line) node)
    (setf (buffer node) buffer)
    buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Edit protocol

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Generic function LINE-COUNT.

(defgeneric line-count (buffer))

(defmethod line-count ((buffer buffer))
  (line-count (contents buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Generic function ITEM-COUNT.

(defgeneric item-count (buffer))

(defmethod item-count ((buffer buffer))
  (item-count (contents buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on function INSERT-ITEM.

(defmethod cluffer:insert-item :after (cursor item)
  (let* ((node (cluffer-internal:dock (line cursor)))
	 (buffer (buffer cursor)))
    (clump-binary-tree:splay node)
    (incf (item-count node))
    (setf (modify-time node) (incf (current-time buffer)))
    (setf (max-modify-time node) (modify-time node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on generic function DELETE-ITEM.

(defmethod cluffer:delete-item :after (cursor)
  (let ((node (cluffer-internal:dock (line cursor)))
	(buffer (buffer cursor)))
    (clump-binary-tree:splay node)
    (decf (item-count node))
    (setf (modify-time node) (incf (current-time buffer)))
    (setf (max-modify-time node) (modify-time node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ERASE-ITEM.

(defgeneric erase-item (cursor))

(defmethod erase-item :after (cursor)
  (let ((node (cluffer-internal:dock (line cursor)))
	(buffer (buffer cursor)))
    (clump-binary-tree:splay node)
    (decf (item-count node))
    (setf (modify-time node) (incf (current-time buffer)))
    (setf (max-modify-time node) (modify-time node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FIND-LINE.

(defgeneric find-line (buffer line-number))

;;; FIXME: This method should not use recursion since the splay tree
;;; is only statistically balanced.  Change it to use iteration
;;; instead.
(defmethod find-line ((buffer buffer) line-number)
  (when (minusp line-number)
    (error 'cluffer:beginning-of-buffer))
  (when (>= line-number (line-count buffer))
    (error 'cluffer:end-of-buffer))
  (labels ((traverse (node line-number)
	     (let* ((left (clump-binary-tree:left node))
		    (right (clump-binary-tree:right node))
		    (left-count (if (null left)
				    0
				    (line-count left))))
	       (cond ((< line-number left-count)
		      (traverse left line-number))
		     ((= line-number left-count)
		      (line node))
		     (t
		      (traverse right (- line-number left-count 1)))))))
    (traverse (contents buffer) line-number)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on generic function LINE-NUMBER.

(defmethod cluffer:line-number (line)
  (let ((node (cluffer-internal:dock line)))
    (clump-binary-tree:splay node)
    (if (null (clump-binary-tree:left node))
	0
	(line-count (clump-binary-tree:left node)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SPLIT-LINE.

(defgeneric split-line (cursor))

;;; This generic function is part of the line-editing protocol, and
;;; should not be used directly by the application.  The application
;;; uses SPLIT-LINE, and SPLIT-LINE calls LINE-SPLIT-LINE.
;;;
;;; This generic function removes all the items to the right of the
;;; cursor in the line in which the cursor is located before the call,
;;; and returns a second line in which those items have been inserted.
;;; SPLIT-LINE must then insert that new line AFTER the one that the
;;; cursor is in before the call.
(defgeneric line-split-line (cursor))

(defmethod split-line (cursor)
  (let* ((existing-line (line cursor))
	 (existing-node (cluffer-internal:dock existing-line))
	 (buffer (buffer existing-node))
	 ;; The number of items that will be removed from the existing
	 ;; line and also the number of items of the new line.
	 (diff (- (cluffer:item-count existing-line)
		  (cluffer:cursor-position cursor))))
    ;; Make sure the existing line is the root of the tree.
    (clump-binary-tree:splay existing-node)
    (decf (item-count existing-node) diff)
    ;; If the cursor is at the end of the line, then the line
    ;; is not modified, but we don't take that into account.
    (let ((time (incf (current-time buffer))))
      (setf (modify-time existing-node) time)
      (setf (max-modify-time existing-node) time))
    (let* ((new-line (line-split-line cursor))
	   (time (incf (current-time buffer)))
	   (new-node (make-instance 'node
		       :buffer buffer
		       :line-count 1
		       :item-count diff
		       :create-time time
		       :modify-time time
		       :max-modify-time time
		       :line new-line)))
      (setf (cluffer-internal:dock new-line) new-node)
      (let ((right-node (clump-binary-tree:right existing-node)))
	(setf (clump-binary-tree:right existing-node) nil)
	(setf (clump-binary-tree:left new-node) existing-node)
	(setf (clump-binary-tree:right new-node) right-node)
	(setf (contents buffer) new-node))))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function JOIN-LINE.

(defgeneric join-line (cursor))

;;; This generic function is part of the line-editing protocol, and
;;; should not be used directly by the application.  The application
;;; uses JOIN-LINE, and JOIN-LINE calls LINE-JOIN-LINE.
;;;
;;; This generic function attaches all of the items of the second line
;;; to the end of the first line.  JOIN-LINE must then delete the
;;; second of these two lines from the tree in the buffer.
(defgeneric line-join-line (line1 line2))

(defmethod join-line (cursor)
  (let ((line (line cursor)))
    (when (cluffer:last-line-p line)
      (error 'cluffer:end-of-buffer))
    (let* ((line-number (cluffer:line-number line))
	   (next-line (find-line (buffer (cluffer-internal:dock line))
				 (1+ line-number))))
      (let ((node-line (cluffer-internal:dock line))
	    (node-next-line (cluffer-internal:dock next-line)))
	(clump-binary-tree:splay node-next-line)
	(clump-binary-tree:splay node-line)
	;; Now LINE is on top and NEXT-LINE is its right child.
	;; Furthermore NEXT-LINE does not have any left children.
	(let ((time (incf (current-time (buffer node-line)))))
	  (setf (modify-time node-line) time)
	  (setf (max-modify-time node-line) time))
	(let ((right (clump-binary-tree:right node-next-line)))
	  (setf (clump-binary-tree:right node-line) nil)
	  (setf (clump-binary-tree:right node-next-line) nil)
	  (setf (clump-binary-tree:right node-line) right))
	(line-join-line line next-line))))
  nil)
