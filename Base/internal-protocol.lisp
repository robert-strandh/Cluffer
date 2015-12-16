(cl:in-package #:cluffer-internal)

;;; This generic function removes all the items to the right of the
;;; cursor in the line in which the cursor is located before the call,
;;; and returns a second line in which those items have been inserted.
(defgeneric split-line (cursor))

(defgeneric join-line (line1 line2))

(defgeneric dock (line))

(defgeneric notify-item-count-changed (dock delta))
