(cl:in-package #:cluffer-test)

(defun random-position (cursor)
  (setf (cluffer:cursor-position cursor)
	(random (1+ (cluffer:item-count cursor)))))

(defun random-insertions ()
  (let ((linea (make-instance 'cluffer-simple-line:line))
	(lineb (make-instance 'cluffer-standard-line:line))
	(cursorla (make-instance
		      'cluffer-simple-line:detached-left-sticky-cursor))
	(cursorra (make-instance
		      'cluffer-simple-line:detached-right-sticky-cursor))
	(cursorlb (make-instance
		      'cluffer-standard-line:detached-left-sticky-cursor))
	(cursorrr (make-instance
		      'cluffer-standard-line:detached-right-sticky-cursor)))
    nil))

