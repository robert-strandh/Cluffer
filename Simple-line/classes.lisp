(cl:in-package #:cluffer-simple-line)

;;;; All lines are represented the same way, namely as a vector of
;;;; objects for the contents and a list of cursors.

(defclass line (cluffer:line)
   ((%contents :initarg :contents :accessor contents)
    (%cursors :initarg :cursors :accessor cursors)))

(defclass attached-cursor ()
  ((%line :initarg :line :accessor line)))

(defclass detached-cursor () ())

(defclass left-sticky-mixin () ())

(defclass right-sticky-mixin () ())

(defclass detached-left-sticky-cursor (detached-cursor left-sticky-mixin)
  ())

(defclass detached-right-sticky-cursor (detached-cursor right-sticky-mixin)
  ())

(defclass closed-left-sticky-cursor
    (attached-cursor
     left-sticky-mixin)
  ())

(defclass closed-right-sticky-cursor
    (attached-cursor
     right-sticky-mixin)
  ())

