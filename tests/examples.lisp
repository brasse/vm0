(in-package :vm0/tests)

(def-suite examples)

(in-suite examples)

(test factorial-works
  (is (equalp
       '("120")
       (capture-output (lambda () (assemble-and-run :program vm0::+factorial-asm+))))))
(test fibonacci-works
  (is (equalp
       '("89")
       (capture-output (lambda () (assemble-and-run :program vm0::+fibonacci-asm+))))))
(test sum-0-n-works
  (is (equalp
       '("15")
       (capture-output (lambda () (assemble-and-run :program vm0::+sum-0-n-asm+))))))
(test max-a-b-works
  (is (equalp
       '("42")
       (capture-output (lambda () (assemble-and-run :program vm0::+max-a-b-asm+))))))
(test function-return-void-works
  (is (equalp
       '("52" "11")
       (capture-output (lambda () (assemble-and-run :program vm0::+function-return-void-asm+))))))
(test function-return-int-works
  (is (equalp
       '("105")
       (capture-output (lambda () (assemble-and-run :program vm0::+function-return-int-asm+))))))
