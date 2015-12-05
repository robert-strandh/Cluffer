(cl:in-package #:cluffer)

(defclass buffer () ())

(defclass line ()
  ((%dock :initarg :dock :initform nil :accessor dock)))

(defclass cursor () ())
