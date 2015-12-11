(cl:in-package #:asdf-user)

(defsystem :cluffer
  :depends-on (:cluffer-base
	       :cluffer-standard-line
	       :cluffer-standard-buffer
	       :cluffer-simple-line)
  :components
  ())
