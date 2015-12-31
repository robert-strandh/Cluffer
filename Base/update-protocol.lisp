(cl:in-package #:cluffer-base)

(defgeneric cluffer:current-time (buffer))

(defgeneric cluffer:update (buffer time sync skip modify create))
