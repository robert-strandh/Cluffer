(cl:in-package #:cluffer-standard-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on function NOTIFY-ITEM-COUNT-CHANGED.

(defmethod cluffer-internal:notify-item-count-changed ((node node) delta)
  (let ((buffer (cluffer-internal:buffer node)))
    (clump-binary-tree:splay node)
    (incf (item-count node) delta)
    (setf (modify-time node) (incf (cluffer:current-time buffer)))
    (setf (max-modify-time node) (modify-time node))))
