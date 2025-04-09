(in-package :vm0/tests)

(def-suite examples)

(in-suite examples)

(test factorial-works
  (is (equalp
       '("120")
       (capture-output (lambda () (assemble-and-run :program +factorial+))))))
(test fibonacci-works
  (is (equalp
       '("89")
       (capture-output (lambda () (assemble-and-run :program +fibonacci+))))))
(test sum-0-n-works
  (is (equalp
       '("15")
       (capture-output (lambda () (assemble-and-run :program +sum-0-n+))))))
(test max-a-b-works
  (is (equalp
       '("42")
       (capture-output (lambda () (assemble-and-run :program +max-a-b+))))))
(test function-return-void-works
  (is (equalp
       '("52" "11")
       (capture-output (lambda () (assemble-and-run :program +function-return-void+))))))
(test function-return-int-works
  (is (equalp
       '("105")
       (capture-output (lambda () (assemble-and-run :program +function-return-int+))))))
