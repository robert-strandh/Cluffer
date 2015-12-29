(cl:in-package #:cluffer-test)

(defmacro assert-error (form error-type)
  `(multiple-value-bind (value error)
       (ignore-errors ,form)
     (declare (ignore value))
     (assert (typep error ',error-type))))
