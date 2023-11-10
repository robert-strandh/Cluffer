(cl:in-package #:cluffer-simple-line)

;;;; All lines are represented the same way, namely as a vector of
;;;; objects for the contents and a list of cursors.

(defclass line (cluffer:line)
   ((%contents :initarg :contents
               :initform (make-array 0)
               :accessor contents)
    (%cursors :initarg :cursors
              :initform '()
              :accessor cursors)))

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
