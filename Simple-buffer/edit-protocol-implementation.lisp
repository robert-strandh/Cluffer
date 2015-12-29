(cl:in-package #:cluffer-simple-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Edit protocol

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Method on generic function LINE-COUNT.

(defmethod cluffer:line-count ((buffer buffer))
  (length (contents buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on generic function ITEM-COUNT.

(defmethod cluffer:item-count ((buffer buffer))
  (loop for node across (contents buffer)
	sum (cluffer:item-count (line node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on generic function FIND-LINE.

(defmethod cluffer:find-line ((buffer buffer) line-number)
  (line (aref (contents buffer) line-number)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on internal generic function BUFFER-LINE-NUMBER.

(defmethod cluffer-internal:buffer-line-number
    ((buffer buffer) (dock node) line)
  (position line (contents buffer) :test #'eq :key #'line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on internal generic function BUFFER-SPLIT-LINE.

(defmethod cluffer-internal:buffer-split-line
    ((buffer buffer) (dock node) (line cluffer:line) position)
  (let* ((line-number (cluffer-internal:buffer-line-number buffer dock line))
	 (contents (contents buffer))
	 (new-contents (make-array (1+ (length contents)))))
    (replace new-contents contents :start1 0
				   :start2 0
				   :end1 (1+ line-number))
    (replace new-contents contents :start1 (+ line-number 2)
				   :start2 (1+ line-number))
    (setf (modify-time dock) (incf (cluffer:current-time buffer)))
    (let* ((new-line (cluffer-internal:line-split-line line position))
	   (time (incf (cluffer:current-time buffer)))
	   (new-node (make-instance 'node
		       :buffer buffer
		       :create-time time
		       :modify-time time
		       :line new-line)))
      (setf (cluffer-internal:dock new-line) new-node)
      (setf (aref new-contents (1+ line-number)) new-node))
    (setf (contents buffer) new-contents))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on internal generic function BUFFER-JOIN-LINE.

(defmethod cluffer-internal:buffer-join-line
    ((buffer buffer) (dock node) (line cluffer:line))
  (let* ((line-number (cluffer-internal:buffer-line-number buffer dock line))
	 (contents (contents buffer))
	 (line-count (length contents)))
    (when (= line-number (1- line-count))
      (error 'cluffer:end-of-buffer))
    (let ((next-line (cluffer:find-line buffer (1+ line-number)))
	  (new-contents (make-array (1- line-count))))
      (cluffer-internal:line-join-line line next-line)
      (replace new-contents contents :start1 0
				     :start2 0
				     :end2 (1+ line-number))
      (replace new-contents contents :start1 (1+ line-number)
				     :start2 (+ line-number 2))
      (setf (modify-time dock) (incf (cluffer:current-time buffer)))
      (setf (contents buffer) new-contents)))
  nil)
