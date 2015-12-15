(cl:in-package #:cluffer)

(defgeneric current-time (buffer))

(defgeneric update (buffer time sync skip modify create))
