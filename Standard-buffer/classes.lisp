(cl:in-package #:cluffer-standard-buffer)

;;; The node contains a reference to the buffer in which it is
;;; located.  This reference is needed because when a node of the tree
;;; is splayed, that node must be explicitly assigned to the CONTENTS
;;; slot of the buffer.
(defclass node (clump-binary-tree:node-with-parent cluffer-internal:dock)
  ((%buffer :initform nil
            :initarg :buffer
            :accessor cluffer-internal:buffer)
   ;; The line count of the entire subtree.
   (%line-count :initarg :line-count :accessor line-count)
   ;; The item count of the entire subtree.
   (%item-count :initarg :item-count :accessor item-count)
   (%create-time :initarg :create-time :reader create-time)
   (%modify-time :initarg :modify-time :accessor modify-time)
   (%max-modify-time :initarg :max-modify-time :accessor max-modify-time)))

;;; CURRENT-TIME represents the time stamp of the last operation in
;;; the buffer.  If UPDATE is called with a time stamp that is greater
;;; than or equal to CURRENT-TIME, then a single SKIP operation is
;;; issued.  Therefore, UPDATE must return the value of CURRENT-TIME,
;;; so that the second of two consecutive calls to UPDATE with the
;;; same time stamp will skip the entire buffer.  The initial line of
;;; a fresh buffer has a CREATE-TIME and a MODIFY-TIME of 0, and the
;;; CURRENT-TIME of a fresh buffer is also 0.  It follows that a TIME
;;; argument of NIL passed to UPDATE must be interpreted as negative
;;; so that a CREATE operation of that initial line is correctly
;;; issued.

(defclass buffer (cluffer:buffer)
  ((%current-time :initform 0 :initarg :current-time
                  :accessor current-time)
   (%contents :initarg :contents :accessor contents)))

(defmethod initialize-instance :after ((buffer buffer) &key initial-line)
  (let ((node (make-instance 'node :buffer buffer
                                   :line-count 1
                                   :item-count 0
                                   :create-time 0
                                   :modify-time 0
                                   :max-modify-time 0
                                   :line initial-line)))
    (setf (contents buffer) node
          (cluffer-internal:dock initial-line) node)))
