(cl:in-package #:cluffer-simple-buffer)

(defclass buffer (cluffer:buffer)
  ((%current-time :initform 0 :initarg :current-time
		  :accessor cluffer:current-time)
   (%contents :initarg :contents :accessor contents)))

(defclass node ()
  ((%buffer :initform nil :initarg :buffer :accessor buffer)
   (%create-time :initarg :create-time :reader create-time)
   (%modify-time :initarg :modify-time :accessor modify-time)
   (%line :initarg :line :accessor line)))

(defmethod initialize-instance :after ((buffer buffer) &key initial-line)
  (setf (contents buffer)
	(vector (make-instance 'node
		  :buffer buffer
		  :create-time 0
		  :modify-time 0
		  :line initial-line))))
