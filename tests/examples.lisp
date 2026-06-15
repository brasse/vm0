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

;; Unary minus lowers to (0 - a), compiling its operand one slot above
;; the pushed 0. Guards against the off-by-one in its one-value check.
(test unary-minus-compiles
  (is (equalp
       '("-5" "-3")
       (capture-output
        (lambda () (compile-and-run '((print (- 5)) (print (- (+ 1 2))))))))))
