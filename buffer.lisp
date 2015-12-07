(cl:in-package #:cluffer-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Line, cursor.

(defclass line ()
  ((%node :initarg :node :initform nil :accessor node)))

(defclass cursor () ())

(defclass detached-cursor (cursor)
  ())

(defclass attached-cursor (cursor)
  ((%line :initarg :line :accessor line)))

(defclass left-sticky-mixin () ())

(defclass right-sticky-mixin () ())

(defclass detached-left-sticky-cursor (detached-cursor left-sticky-mixin)
  ())

(defclass detached-right-sticky-cursor (detached-cursor right-sticky-mixin)
  ())

(defun make-left-sticky-cursor ()
  (make-instance 'detached-left-sticky-cursor))

(defun make-right-sticky-cursor ()
  (make-instance 'detached-right-sticky-cursor))

(defgeneric attach-cursor (cursor line &optional position))

(defgeneric detach-cursor (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Buffer.

(defclass buffer ()
  ((%current-time :initform 0 :initarg :current-time :accessor current-time)
   (%contents :initarg :contents :accessor contents)))

;;; Return the buffer of a node, a cursor, or a line.
(defgeneric buffer (thing))

;;; The node contains a reference to the buffer in which it is
;;; located.  This reference is needed because when a node of the tree
;;; is splayed, that node must be explicitly assigned to the CONTENTS
;;; slot of the buffer.
(defclass node (clump-binary-tree:node-with-parent)
  ((%buffer :initform nil :initarg :buffer :accessor buffer)
   (;; The line count of the entire subtree.
    %line-count :initarg :line-count :accessor line-count)
   (;; The item count of the entire subtree.
    %item-count :initarg :item-count :accessor item-count)
   (%create-time :initarg :create-time :reader create-time)
   (%modify-time :initarg :modify-time :accessor modify-time)
   (%max-modify-time :initarg :max-modify-time :accessor max-modify-time)
   (%line :initarg :line :accessor line)))

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
    (setf (node initial-line) node)
    (setf (buffer node) buffer)
    buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Edit protocol

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function BUFFER
;;;
;;; Given a line or a cursor, return the buffer to which
;;; the line or cursor belongs. 

(defmethod buffer ((line line))
  (buffer (node line)))

(defmethod buffer ((cursor detached-cursor))
  (error 'cluffer:cursor-detached))

(defmethod buffer ((cursor attached-cursor))
  (buffer (line cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function BACKWARD-ITEM.

(defgeneric backward-item (cursor))

(defmethod backward-item ((cursor detached-cursor))
  (error 'cluffer:cursor-detached))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function BEGINNING-OF-LINE.

(defgeneric beginning-of-line (cursor))

(defmethod beginning-of-line ((cursor detached-cursor))
  (error 'cluffer:cursor-detached))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function END-OF-LINE.

(defgeneric end-of-line (cursor))

(defmethod end-of-line ((cursor detached-cursor))
  (error 'cluffer:cursor-detached))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ITEM-BEFORE-CURSOR.

(defgeneric item-before-cursor (cursor))

(defmethod item-before-cursor ((cursor detached-cursor))
  (error 'cluffer:cursor-detached))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ITEM-AFTER-CURSOR.

(defgeneric item-after-cursor (cursor))

(defmethod item-after-cursor ((cursor detached-cursor))
  (error 'cluffer:cursor-detached))

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

(defmethod cluffer:insert-item ((cursor detached-cursor) item)
  (error 'cluffer:cursor-detached))

(defmethod cluffer:insert-item :after ((cursor attached-cursor) item)
  (let* ((node (node (line cursor)))
	 (buffer (buffer cursor)))
    (clump-binary-tree:splay node)
    (incf (item-count node))
    (setf (modify-time node) (incf (current-time buffer)))
    (setf (max-modify-time node) (modify-time node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on generic function DELETE-ITEM.

(defmethod cluffer:delete-item ((cursor detached-cursor))
  (error 'cluffer:cursor-detached))

(defmethod cluffer:delete-item :after ((cursor attached-cursor))
  (let ((node (node (line cursor)))
	(buffer (buffer cursor)))
    (clump-binary-tree:splay node)
    (decf (item-count node))
    (setf (modify-time node) (incf (current-time buffer)))
    (setf (max-modify-time node) (modify-time node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ERASE-ITEM.

(defgeneric erase-item (cursor))

(defmethod erase-item ((cursor detached-cursor))
  (error 'cluffer:cursor-detached))

(defmethod erase-item :after ((cursor attached-cursor))
  (let ((node (node (line cursor)))
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
    (error 'cluffer:begining-of-buffer))
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
  (let ((node (node line)))
    (clump-binary-tree:splay node)
    (if (null (clump-binary-tree:left node))
	0
	(line-count (clump-binary-tree:left node)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FIRST-LINE-P.

(defgeneric first-line-p (line))

(defmethod first-line-p (line)
  (= (line-number line) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function LAST-LINE-P.

(defgeneric last-line-p (line))

(defmethod last-line-p (line)
  (= (line-number line) (1- (line-count (buffer (node line))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function BEGINNING-OF-BUFFER-P.

(defgeneric beginning-of-buffer-p (cursor))

(defmethod beginning-of-buffer-p ((cursor detached-cursor))
  (error 'cluffer:cursor-detached))

(defmethod beginning-of-buffer-p ((cursor attached-cursor))
  (and (beginning-of-line-p cursor)
       (first-line-p (line cursor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function END-OF-BUFFER-P.

(defgeneric end-of-buffer-p (cursor))

(defmethod end-of-buffer-p ((cursor detached-cursor))
  (error 'cluffer:cursor-detached))

(defmethod end-of-buffer-p ((cursor attached-cursor))
  (and (end-of-line-p cursor)
       (last-line-p (line cursor))))

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
	 (existing-node (node existing-line))
	 (buffer (buffer existing-node))
	 ;; The number of items that will be removed from the existing
	 ;; line and also the number of items of the new line.
	 (diff (- (item-count existing-line) (cursor-position cursor))))
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
      (setf (node new-line) new-node)
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

(defmethod join-line ((cursor detached-cursor))
  (error 'cluffer:cursor-detached))

(defmethod join-line ((cursor attached-cursor))
  (let ((line (line cursor)))
    (when (last-line-p line)
      (error 'cluffer:end-of-buffer))
    (let* ((line-number (line-number line))
	   (next-line (find-line (buffer (node line)) (1+ line-number))))
      (let ((node-line (node line))
	    (node-next-line (node next-line)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ITEMS.
;;;
;;; Return the items of a line in a vector.  

(defgeneric items (line &key start end))
