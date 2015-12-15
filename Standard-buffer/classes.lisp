(cl:in-package #:cluffer-standard-buffer)

(defclass buffer (cluffer:buffer)
  ((%current-time :initform 0 :initarg :current-time
		  :accessor cluffer:current-time)
   (%contents :initarg :contents :accessor contents)))

;;; The node contains a reference to the buffer in which it is
;;; located.  This reference is needed because when a node of the tree
;;; is splayed, that node must be explicitly assigned to the CONTENTS
;;; slot of the buffer.
(defclass node (clump-binary-tree:node-with-parent)
  ((%buffer :initform nil :initarg :buffer :accessor buffer)
   (;; The line count of the entire subtree.
    %line-count :initarg :line-count :accessor line-count)
   (;; The item count of the entire subtree.
    %item-count :initarg :item-count :accessor item-count)
   (%create-time :initarg :create-time :reader create-time)
   (%modify-time :initarg :modify-time :accessor modify-time)
   (%max-modify-time :initarg :max-modify-time :accessor max-modify-time)
   (%line :initarg :line :accessor line)))

(defmethod initialize-instance :after ((buffer buffer) &key initial-line)
  (setf (contents buffer)
	(make-instance 'node
	  :buffer buffer
	  :line-count 1
	  :item-count 0
	  :create-time 0
	  :modify-time 0
	  :max-modify-time 0
	  :line initial-line)))
