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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CLOSED-LINE. 
;;; 
;;; The contents of a closed line is a vector of items.  At the
;;; moment, it is always a simple vector.

(defclass closed-line (line)
  ()
  (:default-initargs :contents (vector)))

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
