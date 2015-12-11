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

(defun test-simple-line ()
  (test-simple-line-1)
  (test-simple-line-2)
  (test-simple-line-3))

(defun run-tests ()
  (test-simple-line))
