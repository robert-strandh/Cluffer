(cl:in-package #:cluffer-standard-line)

;;;; A line is either open or closed.  An open line has a
;;;; representation that lends itself to efficient editing.  For a
;;;; closed line, a compact representation is more important.

(defclass line (cluffer:line)
  ((%contents     :initarg  :contents
                  :accessor contents)
   (%cursors      :initarg  :cursors
                  :type     list
                  :accessor cursors
                  :initform '())
   ;; No writer since the first line always remains the first line
   ;; since even splitting the first line at position 0 will insert
   ;; the new line after the first line.
   (%first-line-p :initarg  :first-line-p
                  :reader   cluffer:first-line-p
                  :initform t)
   (%last-line-p  :initarg  :last-line-p
                  :accessor last-line-p
                  :reader   cluffer:last-line-p
                  :initform t)
   ;;; The items of an open line are stored in a gap buffer.
   (%gap-start   :initform 0
                 :initarg :gap-start
                 :accessor gap-start)
   (%gap-end     :initform 1
                 :initarg :gap-end
                 :accessor gap-end)
   (%open-line-p :initform nil
                 :initarg :open-line-p
                 :reader open-line-p
                 :accessor %open-line-p))
  (:default-initargs :contents (vector)))

(defun print-line-contents (contents stream)
  (cond ((stringp contents)
         (let ((length (length contents)))
           (write-char #\" stream)
           (write-string contents stream :end (min 10 length))
           (when (> length 10)
             (write-char #\… stream))
           (write-char #\" stream)))
        (t
         (loop for index from 0 below (min (or *print-length* 3)
                                           (length contents))
               do (prin1 (aref contents index) stream)))))

(defun print-cursor-count (object stream)
  (let ((cursor-count (length (cursors object))))
    (format stream " ~D cursor~:P" cursor-count)))

(defmethod print-object ((object line) stream)
  (if (open-line-p object)
      (let ((contents (contents object)))
        (print-unreadable-object (object stream :type t :identity t)
          (print-line-contents contents stream)
          (print-cursor-count object stream)))
      (let* ((contents (contents object))
             (length (length contents))
             (gap-start (gap-start object))
             (gap-end (gap-end object)))
        (print-unreadable-object (object stream :type t :identity t)
          (print-line-contents (subseq contents 0 gap-start) stream)
          (format stream "[~d]" (- gap-end gap-start))
          (print-line-contents (subseq contents gap-end length) stream)
          (print-cursor-count object stream)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CURSOR.

(defclass cursor (cluffer:cursor)
  ((%line
    :initform nil
    :accessor line
    :reader cluffer:line)
   (%cursor-position
    :accessor cluffer:cursor-position)))

(defmethod initialize-instance :after
    ((cursor cluffer:cursor)
     &key (line nil line-supplied-p)
          (cursor-position 0 cursor-position-supplied-p))
  (cond (line-supplied-p
         (cluffer:attach-cursor cursor line cursor-position))
        (cursor-position-supplied-p
         (error "~@<Cannot supply ~S without also supplying ~S.~@:>"
                :cursor-position :line))))

(defclass left-sticky-cursor (cursor)
  ())

(defclass right-sticky-cursor (cursor)
  ())
