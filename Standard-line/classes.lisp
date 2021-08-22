(cl:in-package #:cluffer-standard-line)

;;;; A line is either open or closed.  An open line has a
;;;; representation that lends itself to efficient editing.  For a
;;;; closed line, a compact representation is more important.

(defclass line (cluffer:line)
   ((%contents :initarg :contents :accessor contents)
    (%cursors :initform '() :initarg :cursors :accessor cursors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class OPEN-LINE.
;;; 
;;; The items of an open line are stored in a gap buffer.

(defclass open-line (line)
  ((%gap-start :initform 0 :initarg :gap-start :accessor gap-start)
   (%gap-end :initform  10 :initarg :gap-end :accessor gap-end))
  (:default-initargs :contents (make-array 10)))

(defmethod print-object ((object open-line) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (loop with contents = (contents object)
          for index from 0 below (gap-start object)
          do (format stream "~s" (aref contents index)))
    (format stream "[~d]" (- (gap-end object) (gap-start object)))
    (loop with contents = (contents object)
          for index from (gap-end object) below (length contents)
          do (format stream "~s" (aref contents index)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CLOSED-LINE. 
;;; 
;;; The contents of a closed line is a vector of items.  At the
;;; moment, it is always a simple vector.

(defclass closed-line (line)
  ()
  (:default-initargs :contents (vector)))

(defmethod print-object ((object closed-line) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (loop with contents = (contents object)
          for index from 0 below (length contents)
          do (format stream "~s" (aref contents index)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CURSOR.

(defclass cursor (cluffer:cursor)
  ((%line
    :initform nil
    :initarg :line
    :accessor line
    :reader cluffer:line)
   (%cursor-position
    :initarg :cursor-position
    :accessor cluffer:cursor-position)))

(defclass left-sticky-cursor (cursor)
  ())

(defclass right-sticky-cursor (cursor)
  ())
