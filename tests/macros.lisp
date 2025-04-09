(in-package :vm0/tests)

(def-suite macros)

(in-suite macros)

(test dup-works
  (is (equalp #((:push 0) (:pick)) (macro #((:dup))))))
(test over-works
  (is (equalp #((:push 1) (:pick)) (macro #((:over))))))
(test swap-works
  (is (equalp #((:push 1) (:roll)) (macro #((:swap))))))
(test rot-works
  (is (equalp #((:push 2) (:roll)) (macro #((:rot))))))
(test inc-works
  (is (equalp #((:push 1) (:add)) (macro #((:inc))))))
(test dec-works
  (is (equalp #((:push 1) (:sub)) (macro #((:dec))))))
(test fn-args-1-works
  (is (equalp #((:push 1) (:roll)) (macro #((:fn-args-1))))))
(test fn-args-2-works
  (is (equalp #((:push 2) (:roll) (:push 2) (:roll)) (macro #((:fn-args-2))))))
(test fn-args-3-works
  (is (equalp #((:push 3) (:roll)
                (:push 3) (:roll)
                (:push 3) (:roll)) (macro #((:fn-args-3))))))
(test retv-works
  (is (equalp #((:push 1) (:roll) (:ret)) (macro #((:retv))))))
