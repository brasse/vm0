(in-package :vm0/tests)

(defun capture-output (thunk)
  (string-trim '(#\NewLine)
               (with-output-to-string (*standard-output*)
                 (funcall thunk))))

(def-suite examples)

(in-suite examples)

(test factorial-works
  (is (string=
       "120"
       (capture-output (lambda () (assemble-and-run :program +factorial+))))))
(test fibonacci-works
  (is (string=
       "89"
       (capture-output (lambda () (assemble-and-run :program +fibonacci+))))))
(test sum-0-n-works
  (is (string=
       "15"
       (capture-output (lambda () (assemble-and-run :program +sum-0-n+))))))
(test max-a-b-works
  (is (string=
       "42"
       (capture-output (lambda () (assemble-and-run :program +max-a-b+))))))
