(cl:in-package #:cluffer-test)

;;; Check that moving backward on an empty simple line signals an
;;; error of type BEGINNING-OF-LINE.
(defun test-simple-line-1 ()
  ;;; Run the test for a left-sticky cursor.
  (let ((line (make-instance 'cluffer-simple-line:line))
	(cursor (make-instance
		    'cluffer-simple-line:detached-left-sticky-cursor)))
    (cluffer:attach-cursor cursor line)
    (assert (typep (nth-value 1 (ignore-errors (cluffer:backward-item cursor)))
		   'cluffer:beginning-of-line)))
  ;;; Run the test for a right-sticky cursor.
  (let ((line (make-instance 'cluffer-simple-line:line))
	(cursor (make-instance
		    'cluffer-simple-line:detached-right-sticky-cursor)))
    (cluffer:attach-cursor cursor line)
    (assert (typep (nth-value 1 (ignore-errors (cluffer:backward-item cursor)))
		   'cluffer:beginning-of-line))))

;;; Check that moving forward on an empty simple line signals an error
;;; of type END-OF-LINE.
(defun test-simple-line-2 ()
  ;;; Run the test for a left-sticky cursor.
  (let ((line (make-instance 'cluffer-simple-line:line))
	(cursor (make-instance
		    'cluffer-simple-line:detached-left-sticky-cursor)))
    (cluffer:attach-cursor cursor line)
    (assert (typep (nth-value 1 (ignore-errors (cluffer:forward-item cursor)))
		   'cluffer:end-of-line)))
  ;;; Run the test for a right-sticky cursor.
  (let ((line (make-instance 'cluffer-simple-line:line))
	(cursor (make-instance
		    'cluffer-simple-line:detached-right-sticky-cursor)))
    (cluffer:attach-cursor cursor line)
    (assert (typep (nth-value 1 (ignore-errors (cluffer:forward-item cursor)))
		   'cluffer:end-of-line))))

;;; Check that the cursor position of a cursor in an empty line is 0.
(defun test-simple-line-3 ()
  ;;; Run the test for a left-sticky cursor.
  (let ((line (make-instance 'cluffer-simple-line:line))
	(cursor (make-instance
		    'cluffer-simple-line:detached-left-sticky-cursor)))
    (cluffer:attach-cursor cursor line)
    (assert (zerop (cluffer:cursor-position cursor))))
  ;;; Run the test for a right-sticky cursor.
  (let ((line (make-instance 'cluffer-simple-line:line))
	(cursor (make-instance
		    'cluffer-simple-line:detached-right-sticky-cursor)))
    (cluffer:attach-cursor cursor line)
    (assert (zerop (cluffer:cursor-position cursor)))))

;;; Check that BEGINNING-OF-LINE-P returns true for a cursor in an
;;; empty line.
(defun test-simple-line-4 ()
  ;;; Run the test for a left-sticky cursor.
  (let ((line (make-instance 'cluffer-simple-line:line))
	(cursor (make-instance
		    'cluffer-simple-line:detached-left-sticky-cursor)))
    (cluffer:attach-cursor cursor line)
    (assert (cluffer:beginning-of-line-p cursor)))
  ;;; Run the test for a right-sticky cursor.
  (let ((line (make-instance 'cluffer-simple-line:line))
	(cursor (make-instance
		    'cluffer-simple-line:detached-right-sticky-cursor)))
    (cluffer:attach-cursor cursor line)
    (assert (cluffer:beginning-of-line-p cursor))))

;;; Check that END-OF-LINE-P returns true for a cursor in an
;;; empty line.
(defun test-simple-line-5 ()
  ;;; Run the test for a left-sticky cursor.
  (let ((line (make-instance 'cluffer-simple-line:line))
	(cursor (make-instance
		    'cluffer-simple-line:detached-left-sticky-cursor)))
    (cluffer:attach-cursor cursor line)
    (assert (cluffer:end-of-line-p cursor)))
  ;;; Run the test for a right-sticky cursor.
  (let ((line (make-instance 'cluffer-simple-line:line))
	(cursor (make-instance
		    'cluffer-simple-line:detached-right-sticky-cursor)))
    (cluffer:attach-cursor cursor line)
    (assert (cluffer:end-of-line-p cursor))))

;;; Check that ITEM-COUNT returns 0 for an empty line.
(defun test-simple-line-6 ()
  (let ((line (make-instance 'cluffer-simple-line:line)))
    (assert (zerop (cluffer:item-count line)))))

;;; Check that calling ITEM-BEFORE-CURSOR on an empty simple line
;;; signals an error of type BEGINNING-OF-LINE.
(defun test-simple-line-7 ()
  ;;; Run the test for a left-sticky cursor.
  (let ((line (make-instance 'cluffer-simple-line:line))
	(cursor (make-instance
		    'cluffer-simple-line:detached-left-sticky-cursor)))
    (cluffer:attach-cursor cursor line)
    (assert (typep (nth-value 1 (ignore-errors (cluffer:item-before-cursor cursor)))
		   'cluffer:beginning-of-line)))
  ;;; Run the test for a right-sticky cursor.
  (let ((line (make-instance 'cluffer-simple-line:line))
	(cursor (make-instance
		    'cluffer-simple-line:detached-right-sticky-cursor)))
    (cluffer:attach-cursor cursor line)
    (assert (typep (nth-value 1 (ignore-errors (cluffer:item-before-cursor cursor)))
		   'cluffer:beginning-of-line))))

;;; Check that calling ITEM-AFTER-CURSOR on an empty simple line
;;; signals an error of type BEGINNING-OF-LINE.
(defun test-simple-line-8 ()
  ;;; Run the test for a left-sticky cursor.
  (let ((line (make-instance 'cluffer-simple-line:line))
	(cursor (make-instance
		    'cluffer-simple-line:detached-left-sticky-cursor)))
    (cluffer:attach-cursor cursor line)
    (assert (typep (nth-value 1 (ignore-errors (cluffer:item-after-cursor cursor)))
		   'cluffer:end-of-line)))
  ;;; Run the test for a right-sticky cursor.
  (let ((line (make-instance 'cluffer-simple-line:line))
	(cursor (make-instance
		    'cluffer-simple-line:detached-right-sticky-cursor)))
    (cluffer:attach-cursor cursor line)
    (assert (typep (nth-value 1 (ignore-errors (cluffer:item-after-cursor cursor)))
		   'cluffer:end-of-line))))

;;; Check that CURSOR-ATTACHED-P returns true if and only if the
;;; cursor is attached.
(defun test-simple-line-9 ()
  ;;; Run the test for a left-sticky cursor.
  (let ((line (make-instance 'cluffer-simple-line:line))
	(cursor (make-instance
		    'cluffer-simple-line:detached-left-sticky-cursor)))
    (assert (not (cluffer:cursor-attached-p cursor)))
    (cluffer:attach-cursor cursor line)
    (assert (cluffer:cursor-attached-p cursor)))
  ;;; Run the test for a right-sticky cursor.
  (let ((line (make-instance 'cluffer-simple-line:line))
	(cursor (make-instance
		    'cluffer-simple-line:detached-right-sticky-cursor)))
    (assert (not (cluffer:cursor-attached-p cursor)))
    (cluffer:attach-cursor cursor line)
    (assert (cluffer:cursor-attached-p cursor))))

(defun test-simple-line-10 ()
  (let ((line (make-instance 'cluffer-simple-line:line))
	(lc (make-instance 'cluffer-simple-line:detached-left-sticky-cursor))
	(rc (make-instance 'cluffer-simple-line:detached-right-sticky-cursor)))
    (cluffer:attach-cursor lc line)
    (cluffer:attach-cursor rc line)
    (cluffer:insert-item lc 234)
    (assert (cluffer:beginning-of-line-p lc))
    (assert (zerop (cluffer:cursor-position lc)))
    (assert (not (cluffer:beginning-of-line-p rc)))
    (assert (not (zerop (cluffer:cursor-position rc))))
    (assert (cluffer:end-of-line-p rc))
    (assert (= (cluffer:cursor-position rc) 1))
    (assert (not (cluffer:end-of-line-p lc)))
    (assert (eql (cluffer:item-after-cursor lc) 234))
    (assert (eql (cluffer:item-before-cursor rc) 234))
    (assert (equalp (cluffer:items line) #(234)))
    (assert (= (cluffer:item-count line) 1))
    (assert (= (cluffer:item-count lc) 1))
    (assert (= (cluffer:item-count rc) 1))))

(defun test-simple-line ()
  (test-simple-line-1)
  (test-simple-line-2)
  (test-simple-line-3)
  (test-simple-line-4)
  (test-simple-line-5)
  (test-simple-line-6)
  (test-simple-line-7)
  (test-simple-line-8)
  (test-simple-line-9)
  (test-simple-line-10))