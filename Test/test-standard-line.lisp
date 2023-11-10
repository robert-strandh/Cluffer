(cl:in-package #:cluffer-test)

(defun random-position (cursor)
  (setf (cluffer:cursor-position cursor)
        (random (1+ (cluffer:item-count cursor)))))

(defun random-insertions-iteration ()
  (let ((linea (make-instance 'cluffer-simple-line:line))
        (lineb (make-instance 'cluffer-standard-line:closed-line))
        (cursorla (make-instance
                      'cluffer-simple-line:left-sticky-cursor))
        (cursorra (make-instance
                      'cluffer-simple-line:right-sticky-cursor))
        (cursorlb (make-instance
                      'cluffer-standard-line:left-sticky-cursor))
        (cursorrb (make-instance
                      'cluffer-standard-line:right-sticky-cursor)))
    (cluffer:attach-cursor cursorla linea)
    (cluffer:attach-cursor cursorra linea)
    (cluffer:attach-cursor cursorlb lineb)
    (cluffer:attach-cursor cursorrb lineb)
    (loop for size from 0 to 1000
          for pos1 = (random (1+ size))
          for pos2 = (random (1+ size))
          for item = (random 100000)
          do (setf (cluffer:cursor-position cursorla) pos1)
             (setf (cluffer:cursor-position cursorlb) pos1)
             (setf (cluffer:cursor-position cursorra) pos2)
             (setf (cluffer:cursor-position cursorrb) pos2)
             (if (zerop (random 2))
                 (progn (cluffer:insert-item cursorla item)
                        (cluffer:insert-item cursorlb item))
                 (progn (cluffer:insert-item cursorra item)
                        (cluffer:insert-item cursorrb item)))
             (assert (= (cluffer:cursor-position cursorla)
                        (cluffer:cursor-position cursorlb)))
             (assert (= (cluffer:cursor-position cursorra)
                        (cluffer:cursor-position cursorrb)))
             (assert (equalp (cluffer:items cursorla)
                             (cluffer:items cursorlb))))))

(defun random-insertions ()
  (loop repeat 100
        do (random-insertions-iteration)))

(defun random-deletions-iteration ()
  (let ((linea (make-instance 'cluffer-simple-line:line))
        (lineb (make-instance 'cluffer-standard-line:closed-line))
        (cursorla (make-instance
                      'cluffer-simple-line:left-sticky-cursor))
        (cursorra (make-instance
                      'cluffer-simple-line:right-sticky-cursor))
        (cursorlb (make-instance
                      'cluffer-standard-line:left-sticky-cursor))
        (cursorrb (make-instance
                      'cluffer-standard-line:right-sticky-cursor)))
    (cluffer:attach-cursor cursorla linea)
    (cluffer:attach-cursor cursorra linea)
    (cluffer:attach-cursor cursorlb lineb)
    (cluffer:attach-cursor cursorrb lineb)
    (loop repeat 1000
          for item = (random 100000)
          do (cluffer:insert-item cursorla item)
             (cluffer:insert-item cursorlb item))
    (loop for size from 1000 above 0
          for pos1 = (random size)
          for pos2 = (random size)
          do (setf (cluffer:cursor-position cursorla) pos1)
             (setf (cluffer:cursor-position cursorlb) pos1)
             (setf (cluffer:cursor-position cursorra) pos2)
             (setf (cluffer:cursor-position cursorrb) pos2)
             (if (zerop (random 2))
                 (progn (cluffer:delete-item cursorla)
                        (cluffer:delete-item cursorlb))
                 (progn (cluffer:delete-item cursorra)
                        (cluffer:delete-item cursorrb)))
             (assert (= (cluffer:cursor-position cursorla)
                        (cluffer:cursor-position cursorlb)))
             (assert (= (cluffer:cursor-position cursorra)
                        (cluffer:cursor-position cursorrb)))
             (assert (equalp (cluffer:items cursorla)
                             (cluffer:items cursorlb))))))

(defun random-deletions ()
  (loop repeat 100
        do (random-deletions-iteration)))

(defun test-standard-line ()
  (format *trace-output* "~&; Standard line test~%")
  (random-insertions)
  (random-deletions))
