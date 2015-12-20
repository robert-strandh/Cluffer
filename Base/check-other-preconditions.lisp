(cl:in-package #:cluffer-base)

(defmethod (setf cluffer:cursor-position) :before (new-position cursor)
  (when (minusp new-position)
    (error 'cluffer:beginning-of-line))
  (when (> new-position (cluffer:item-count (cluffer:line cursor)))
    (error 'cluffer:end-of-line)))

(defmethod cluffer:item-at-position :before
    ((line cluffer:line) position)
  (when (minusp position)
    (error 'cluffer:beginning-of-line))
  (when (>= position (cluffer:item-count line))
    (error 'cluffer:end-of-line)))

(defmethod cluffer:insert-item-at-position :before
    ((line cluffer:line) item position)
  (when (minusp position)
    (error 'cluffer:beginning-of-line))
  (when (> position (cluffer:item-count line))
    (error 'cluffer:end-of-line)))

(defmethod cluffer:delete-item-at-position :before
    ((line cluffer:line) position)
  (when (minusp position)
    (error 'cluffer:beginning-of-line))
  (when (>= position (cluffer:item-count line))
    (error 'cluffer:end-of-line)))

(defmethod cluffer:find-line :before ((buffer cluffer:buffer) line-number)
  (when (minusp line-number)
    (error 'cluffer:beginning-of-buffer))
  (when (>= line-number (cluffer:line-count buffer))
    (error 'cluffer:end-of-buffer)))

(defmethod cluffer:attach-cursor :before
    ((cursor cluffer:cursor) (line cluffer:line) &optional position)
  (when (cluffer:cursor-attached-p cursor)
    (error 'cursor-attached)))
