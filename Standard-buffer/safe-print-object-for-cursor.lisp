(cl:in-package #:cluffer-standard-buffer)

;;;; The purpose of the code in this file is to provide a
;;;; `print-object' method for the `cluffer-standard-line:cursor'
;;;; class that displays the line number of the line to which the
;;;; cursor is attached without disturbing any data structures. The
;;;; latter property is relevant when a `cluffer-standard-line:cursor'
;;;; is used in the context of a `cluffer-standard-buffer:buffer'
;;;; because the default implementation of `cluffer:line-number' for
;;;; that combination can cause splay operations on the splay tree of
;;;; the buffer.

(defun safe-line-number (node)
  ;; The number of lines "before" a node is the sum of
  ;; 1) the number of lines in the left subtree of the node (therefore
  ;;    RIGHT-CHILD-P is fixed at true for the initial
  ;;    worklist item)
  ;; 2) Certain line counts along the path from the node to the root:
  ;;    for each ancestor, if the path reaches the ancestor through
  ;;    its right child, then the number of lines in the left subtree
  ;;    of that ancestor since all of those lines are "before" the
  ;;    the right child.
  ;; The following implementation is iterative as to not overflow the
  ;; stack for large, poorly balanced trees.
  (labels ((total-line-count (node)
             (loop with count = 0
                   with worklist = (list node)
                   while worklist
                   do (let* ((node  (pop worklist))
                             (left  (bt:left node))
                             (right (bt:right node)))
                        (incf count)
                        (unless (null left)
                          (push left worklist))
                        (unless (null right)
                          (push right worklist)))
                   finally (return count))))
    (loop with count   = 0
          with worklist   = (list (cons node t))
          for  element = (pop worklist)
          while element
          do (destructuring-bind (node . right-child-p) element
               (when right-child-p
                 (incf count)
                 (let ((left (bt:left node)))
                   (unless (null left)
                     (incf count (total-line-count left)))))
               (let ((parent (bt:parent node)))
                 (unless (null parent)
                   (push (cons parent (eq node (bt:right parent)))
                         worklist))))
          finally (return (1- count)))))

(defun maybe-safe-line-number (cursor)
  (let* ((line (cluffer:line cursor))
         (node (cluffer-internal:dock line)))
    (typecase node
      (node (safe-line-number node))
      (t    (cluffer:line-number line)))))

(defmethod print-object :around ((object cluffer:cursor) stream)
  (if (cluffer:cursor-attached-p object)
      (print-unreadable-object (object stream :type t :identity t)
        (format stream "~:D:~:D"
                (maybe-safe-line-number object)
                (cluffer:cursor-position object)))
      (call-next-method)))
