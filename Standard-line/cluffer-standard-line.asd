(cl:in-package #:asdf-user)

(defsystem :cluffer-standard-line
  :depends-on (:cluffer-base)
  :serial t
  :components
  ((:file "packages")))
