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
;;; Method on generic function LINE-COUNT.

(defmethod cluffer:line-count ((buffer buffer))
  (line-count (contents buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Method on generic function ITEM-COUNT.

(defmethod cluffer:item-count ((buffer buffer))
  (item-count (contents buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on generic function FIND-LINE.

(defmethod cluffer:find-line ((buffer buffer) line-number)
  (when (minusp line-number)
    (error 'cluffer:beginning-of-buffer))
  (when (>= line-number (line-count buffer))
    (error 'cluffer:end-of-buffer))
  (loop with node = (contents buffer)
	with relative-line-number = line-number
	for left = (clump-binary-tree:left node)
	for right = (clump-binary-tree:right node)
	for left-count = (if (null left) 0 (line-count left))
	until (= relative-line-number left-count)
	do (if (< relative-line-number left-count)
	       (setf node left)
	       (progn (setf node right)
		      (decf relative-line-number (1+ left-count))))
	finally (return (line node))))

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
;;; Method on generic function SPLIT-LINE.

(defmethod cluffer:split-line (cursor)
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
    (let* ((new-line (cluffer-internal:split-line cursor))
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
;;; Method on generic function JOIN-LINE.

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
