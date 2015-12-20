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
  ((%gap-start :initarg :gap-start :accessor gap-start)
   (%gap-end :initarg :gap-end :accessor gap-end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CLOSED-LINE. 
;;; 
;;; The contents of a closed line is a vector of items.  At the
;;; moment, it is always a simple vector.

(defclass closed-line (line)
  ()
  (:default-initargs :contents (vector)))

(defclass attached-cursor (cluffer:cursor)
  ((%line :initarg :line :accessor cluffer:line)))

(defmethod cluffer:cursor-attached-p ((cursor attached-cursor))
  t)

(defclass detached-cursor (cluffer:cursor) ())

(defmethod cluffer:cursor-attached-p ((cursor detached-cursor))
  nil)

(defclass left-sticky-mixin () ())

(defclass right-sticky-mixin () ())

(defclass detached-left-sticky-cursor (detached-cursor left-sticky-mixin)
  ())

(defclass detached-right-sticky-cursor (detached-cursor right-sticky-mixin)
  ())

(defclass left-sticky-cursor (attached-cursor left-sticky-mixin)
  ())

(defclass right-sticky-cursor (attached-cursor right-sticky-mixin)
  ())
