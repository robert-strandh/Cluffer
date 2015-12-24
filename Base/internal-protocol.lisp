(cl:in-package #:cluffer-internal)

;;; This class is the base class for all classes representing points
;;; in a buffer structure where lines are attached.
(defclass dock ()
  ((%line :initarg :line :reader line)))

;;; This generic function removes all the items to the right of
;;; POSITION in LINE, and returns a second line in which those items
;;; have been inserted.  Cursors that are located at positions
;;; strictly greater than POSITION are moved to the new line.  Cursors
;;; that are located at positions strictly less than POSITION remain
;;; in their old positions.  What happens to cursors located at
;;; POSITION depends on the exact type of the line and the exact type
;;; of those cursors.  The STANDARD-LINE implementation provides two
;;; types of cursors, namely left-sticky and right-sticky cursors.
;;; For this implementation, left-sticky cursors located at POSITION
;;; remain in LINE and right-sticky cursors located at POSITION are
;;; moved to the beginning of the new line.
(defgeneric split-line (line position))

;;; This generic function attaches all of the items of the second line
;;; to the end of the first line.
(defgeneric join-line (line1 line2))

;;; Given a line, return a DOCK object that identifies the position of
;;; LINE in the buffer to which LINE belongs.  If LINE does not belong
;;; to any buffer, then this function returns NIL.  The exact nature
;;; of the dock object depends on the type of the buffer to which LINE
;;; belongs.
(defgeneric dock (line))

(defgeneric notify-item-count-changed (dock delta))

;;; Given a DOCK object, return the buffer to which that dock object
;;; belongs.
(defgeneric buffer (dock))

;;; This generic function is called by the default method on the
;;; external generic function LINE-NUMBER (specialized to LINE),
;;; passing the result of calling the generic function DOCK on the
;;; line and the line itself as arguments.
(defgeneric dock-line-number (dock line))

;;; This generic function is called by the default method on the
;;; internal generic function DOCK-LINE-NUMBER (specialized to DOCK),
;;; passing the result of calling the internal generic function BUFFER
;;; on the DOCK object, the dock object itself, and the line.
(defgeneric buffer-line-number (buffer dock line))

(defgeneric dock-split-line (dock line position))

(defgeneric buffer-split-line (buffer dock line position))
