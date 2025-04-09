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
