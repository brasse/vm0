(in-package :vm0/tests)

(defun capture-output (thunk)
  (string-trim '(#\NewLine)
               (with-output-to-string (*standard-output*)
                 (funcall thunk))))

(defmacro stack-test (name expected program)
  `(test ,name
     (let ((stack (make-array 10 :initial-element :empty)))
       (run-program ,program :stack stack)
       (let ((stack-prefix (subseq stack 0 (length ,expected))))
         (is (equalp ,expected stack-prefix)
           "expected ~S, but got ~S" ,expected stack-prefix)))))

(def-suite core-instructions)

(in-suite core-instructions)

(stack-test push-works
  #(10)
  #((:push 10)))
(stack-test pop-works
  #()
  #((:push 10) (:pop)))
(stack-test pick-works
  #(1 2 3 1)
  #((:push 1) (:push 2) (:push 3) (:push 2) (:pick)))
(stack-test roll-works
  #(2 3 1)
  #((:push 1) (:push 2) (:push 3) (:push 2) (:roll)))
(stack-test add-works
  #(42)
  #((:push 32) (:push 10) (:add)))
(stack-test sub-works
  #(22)
  #((:push 32) (:push 10) (:sub)))
(stack-test mul-works
  #(15)
  #((:push 3) (:push 5) (:mul)))
(stack-test div-works
  #(4)
  #((:push 13) (:push 3) (:div)))
(stack-test eq-works-when-eg
  #(1)
  #((:push 10) (:push 10) (:eq)))
(stack-test eq-works-when-not-eq
  #(0)
  #((:push 11) (:push 10) (:eq)))
(stack-test lt-works-when-lt
  #(1)
  #((:push 9) (:push 10) (:lt)))
(stack-test lt-works-when-not-lt
  #(0)
  #((:push 10) (:push 10) (:lt)))
(stack-test gt-works-when-gt
  #(1)
  #((:push 11) (:push 10) (:gt)))
(stack-test gt-works-when-not-gt
  #(0)
  #((:push 10) (:push 10) (:gt)))
(stack-test jmp-works
  #(42)
  #((:jmp 2) (:push 10) (:push 42)))
(stack-test jz-works-when-jump
  #(42)
  #((:push 0) (:jz 3) (:push 10) (:push 42)))
(stack-test jz-works-when-not-jump
  #(10 42)
  #((:push 1) (:jz 3) (:push 10) (:push 42)))
(stack-test jnz-works-when-jump
  #(42)
  #((:push 1) (:jnz 3) (:push 10) (:push 42)))
(stack-test jnz-works-when-not-jump
  #(10 42)
  #((:push 0) (:jnz 3) (:push 10) (:push 42)))
(stack-test halt-works
  #(10)
  #((:push 10) (:halt) (:push 20)))
(stack-test print-pops
  #()
  #((:push 10) (:print)))

(test print-outputs
  (is (string=
       "42"
       (capture-output (lambda () (run-program #((:push 42) (:print))))))))
