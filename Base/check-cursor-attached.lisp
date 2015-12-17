(cl:in-package #:cluffer-base)

(defmacro check-cursor-attached (name arguments)
  (let ((args (substitute '(cursor cluffer:cursor) 'cursor arguments)))
    `(defmethod ,name :before ,args
       (declare (ignore ,@(remove 'cursor arguments)))
       (unless (cluffer:cursor-attached-p cursor)
	 (error 'cluffer:cursor-detached)))))

;;; This :BEFORE method checks whether the cursor is attached, and if
;;; not, signals an error.
(defmethod cluffer:cursor-position :before ((cursor cluffer:cursor))
  (unless (cluffer:cursor-attached-p cursor)
    (error 'cluffer:cursor-detached)))

;;; This :BEFORE method checks whether the cursor is attached, and if
;;; not, signals an error.
(defmethod (setf cluffer:cursor-position) :before (position (cursor cluffer:cursor))
  (declare (ignore position))
  (unless (cluffer:cursor-attached-p cursor)
    (error 'cluffer:cursor-detached)))

;;; This :BEFORE method checks whether the cursor is attached, and if
;;; not, signals an error.
(defmethod cluffer:beginning-of-line-p :before ((cursor cluffer:cursor))
  (unless (cluffer:cursor-attached-p cursor)
    (error 'cluffer:cursor-detached)))

;;; This :BEFORE method checks whether the cursor is attached, and if
;;; not, signals an error.
(defmethod cluffer:end-of-line-p :before ((cursor cluffer:cursor))
  (unless (cluffer:cursor-attached-p cursor)
    (error 'cluffer:cursor-detached)))

;;; This :BEFORE method checks whether the cursor is attached, and if
;;; not, signals an error.
(defmethod cluffer:forward-item :before ((cursor cluffer:cursor))
  (unless (cluffer:cursor-attached-p cursor)
    (error 'cluffer:cursor-detached)))

;;; This :BEFORE method checks whether the cursor is attached, and if
;;; not, signals an error.
(defmethod cluffer:backward-item :before ((cursor cluffer:cursor))
  (unless (cluffer:cursor-attached-p cursor)
    (error 'cluffer:cursor-detached)))

;;; This :BEFORE method checks whether the cursor is attached, and if
;;; not, signals an error.
(defmethod cluffer:beginning-of-line :before ((cursor cluffer:cursor))
  (unless (cluffer:cursor-attached-p cursor)
    (error 'cluffer:cursor-detached)))

;;; This :BEFORE method checks whether the cursor is attached, and if
;;; not, signals an error.
(defmethod cluffer:end-of-line :before ((cursor cluffer:cursor))
  (unless (cluffer:cursor-attached-p cursor)
    (error 'cluffer:cursor-detached)))

;;; This :BEFORE method checks whether the cursor is attached, and if
;;; not, signals an error.
(defmethod cluffer:item-before-cursor :before ((cursor cluffer:cursor))
  (unless (cluffer:cursor-attached-p cursor)
    (error 'cluffer:cursor-detached)))

;;; This :BEFORE method checks whether the cursor is attached, and if
;;; not, signals an error.
(defmethod cluffer:item-after-cursor :before ((cursor cluffer:cursor))
  (unless (cluffer:cursor-attached-p cursor)
    (error 'cluffer:cursor-detached)))

;;; This :BEFORE method checks whether the cursor is attached, and if
;;; not, signals an error.
(defmethod cluffer:insert-item :before ((cursor cluffer:cursor) item)
  (declare (ignore item))
  (unless (cluffer:cursor-attached-p cursor)
    (error 'cluffer:cursor-detached)))

;;; This :BEFORE method checks whether the cursor is attached, and if
;;; not, signals an error.
(defmethod cluffer:delete-item :before ((cursor cluffer:cursor))
  (unless (cluffer:cursor-attached-p cursor)
    (error 'cluffer:cursor-detached)))

;;; This :BEFORE method checks whether the cursor is attached, and if
;;; not, signals an error.
(defmethod cluffer:erase-item :before ((cursor cluffer:cursor))
  (unless (cluffer:cursor-attached-p cursor)
    (error 'cluffer:cursor-detached)))

;;; This :BEFORE method checks whether the cursor is attached, and if
;;; not, signals an error.
(defmethod cluffer:beginning-of-buffer-p :before ((cursor cluffer:cursor))
  (unless (cluffer:cursor-attached-p cursor)
    (error 'cluffer:cursor-detached)))

;;; This :BEFORE method checks whether the cursor is attached, and if
;;; not, signals an error.
(defmethod cluffer:end-of-buffer-p :before ((cursor cluffer:cursor))
  (unless (cluffer:cursor-attached-p cursor)
    (error 'cluffer:cursor-detached)))

;;; This :BEFORE method checks whether the cursor is attached, and if
;;; not, signals an error.
(defmethod cluffer:split-line :before ((cursor cluffer:cursor))
  (unless (cluffer:cursor-attached-p cursor)
    (error 'cluffer:cursor-detached)))

;;; This :BEFORE method checks whether the cursor is attached, and if
;;; not, signals an error.
(defmethod cluffer:join-line :before ((cursor cluffer:cursor))
  (unless (cluffer:cursor-attached-p cursor)
    (error 'cluffer:cursor-detached)))

(defmethod cluffer:buffer :before ((cursor cluffer:cursor))
  (unless (cluffer:cursor-attached-p cursor)
    (error 'cluffer:cursor-detached)))

(defmethod cluffer:line :before ((cursor cluffer:cursor))
  (unless (cluffer:cursor-attached-p cursor)
    (error 'cluffer:cursor-detached)))

(defmethod cluffer:items :before ((cursor cluffer:cursor) &key start end)
  (declare (ignore start end))
  (unless (cluffer:cursor-attached-p cursor)
    (error 'cluffer:cursor-detached)))
