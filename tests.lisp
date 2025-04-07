(defpackage :vm0/tests
  (:use :cl :fiveam :vm0/programs))

(in-package :vm0/tests)

(defun capture-output (thunk)
  (string-trim '(#\NewLine)
               (with-output-to-string (*standard-output*)
                 (funcall thunk))))

(def-suite programs)

(in-suite programs)

(test factorial-works
  (is (string= "120" (capture-output #'vm0/programs:factorial))))
(test fibonacci-works
  (is (string= "89" (capture-output #'vm0/programs:fibonacci))))
(test sum-0-n-works
  (is (string= "15" (capture-output #'vm0/programs:sum-0-n))))
(test max-a-b-works
  (is (string= "42" (capture-output #'vm0/programs:max-a-b))))
