(cl:in-package #:cluffer-internal)

(defgeneric split-line (cursor))

(defgeneric join-line (line1 line2))

(defgeneric dock (line))

(defgeneric notify-item-count-changed (buffer dock delta))
