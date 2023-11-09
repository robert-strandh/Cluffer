(cl:in-package #:common-lisp-user)

(defpackage #:cluffer-test
  (:use #:common-lisp)
  (:shadow #:delete)
  (:export #:run-tests))
