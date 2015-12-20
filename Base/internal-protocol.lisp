(cl:in-package #:cluffer-internal)

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

(defgeneric dock (line))

(defgeneric notify-item-count-changed (dock delta))
