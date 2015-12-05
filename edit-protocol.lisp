(cl:in-package #:cluffer)

(defclass buffer () ())

(defclass line ()
  ((%dock :initarg :dock :initform nil :accessor dock)))

(defclass cursor () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function CURSOR-POSITION.
;;;
;;; Given a cursor, return its conceptual position.

(defgeneric cursor-position (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF CURSOR-POSITION).
;;;
;;; Given a cursor, set its conceptual position.

(defgeneric (setf cursor-position) (position cursor))
