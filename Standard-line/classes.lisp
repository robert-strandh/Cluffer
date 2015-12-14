(cl:in-package #:cluffer-standard-line)

;;;; A line is either open or closed.  An open line has a
;;;; representation that lends itself to efficient editing.  For a
;;;; closed line, a compact representation is more important.  The
;;;; cursors attached to an open line are open cursors.  Similarly,
;;;; the cursors attached to a closed line are closed cursors.

(defclass line (cluffer:line)
   ((%contents :initarg :contents :accessor contents)
    (%cursors :initarg :cursors :accessor cursors)))

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
  ((%line :initarg :line :accessor line)))

(defclass detached-cursor (cluffer:cursor) ())

(defclass left-sticky-mixin () ())

(defclass right-sticky-mixin () ())

(defclass detached-left-sticky-cursor (detached-cursor left-sticky-mixin)
  ())

(defclass detached-right-sticky-cursor (detached-cursor right-sticky-mixin)
  ())

;;; At the moment, we represent open cursors and a closed cursors the
;;; same way, where the position is the logical position in the line.
;;; Later, we might represent open cursors by the physical position in
;;; the gap vector so as to avoid updating cursors when items are
;;; inserted.  On the other hand, that would be an important
;;; optimization only if the number of cursors is very large.
(defclass open-cursor-mixin ()
  ((%cursor-position
    :initarg :cursor-position
    :accessor cluffer:cursor-position)))

(defclass closed-cursor-mixin ()
  ((%cursor-position
    :initarg :cursor-position
    :accessor cluffer:cursor-position)))

(defclass closed-left-sticky-cursor
    (attached-cursor
     closed-cursor-mixin
     left-sticky-mixin)
  ())

(defclass closed-right-sticky-cursor
    (attached-cursor
     closed-cursor-mixin
     right-sticky-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class OPEN-LEFT-STICKY CURSOR.

(defclass open-left-sticky-cursor
    (attached-cursor
     open-cursor-mixin
     left-sticky-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class OPEN-RIGHT-STICKY CURSOR.

(defclass open-right-sticky-cursor
    (attached-cursor
     open-cursor-mixin
     right-sticky-mixin)
  ())

