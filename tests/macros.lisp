(in-package :vm0/tests)

(def-suite macros)

(in-suite macros)

(test swap-works
  (is (equalp #((:push 1) (:roll)) (macro #((:swap))))))
(test rot-works
  (is (equalp #((:push 2) (:roll)) (macro #((:rot))))))
(test inc-works
  (is (equalp #((:push 1) (:add)) (macro #((:inc))))))
(test dec-works
  (is (equalp #((:push 1) (:sub)) (macro #((:dec))))))
(test lte-works
  (is (equalp #((:gt) (:not)) (macro #((:lte))))))
(test gte-works
  (is (equalp #((:lt) (:not)) (macro #((:gte))))))
(test fn-args-1-works
  (is (equalp #((:push 2) (:iroll) (:push 2) (:iroll)) (macro #((:fn-args-1))))))
(test fn-args-2-works
  (is (equalp #((:push 3) (:iroll) (:push 3) (:iroll)) (macro #((:fn-args-2))))))
(test fn-args-3-works
  (is (equalp #((:push 4) (:iroll) (:push 4) (:iroll)) (macro #((:fn-args-3))))))
(test retv-works
  (is (equalp #((:push 2) (:iroll) (:ret)) (macro #((:retv))))))
