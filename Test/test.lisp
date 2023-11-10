(cl:in-package #:cluffer-test)

(defun run-tests ()
  (test-simple-line)
  (test-standard-line)
  (test-simple-buffer)
  (test-standard-buffer)
  (test-update)
  (test-edit-protocol))
