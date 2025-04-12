(in-package :vm0/tests)

(def-suite stack-frames)

(in-suite stack-frames)

(defparameter f0 (vm0::make-frame))
(vm0::frame-add-binding f0 'a)
(vm0::frame-add-binding f0 'b)
(vm0::frame-add-binding f0 'c)


(defparameter f1 (vm0::make-frame))
(vm0::frame-add-binding f1 'foo)
(vm0::frame-add-binding f1 'bar)
(vm0::frame-add-binding f1 'baz)
(setf (vm0::frame-offset f1) 4)

(defparameter stack-frames (list f1 f0))

(test get-depth-works
  (is (= 4 (vm0::get-depth stack-frames 'foo)))
  (is (= 5 (vm0::get-depth stack-frames 'bar)))
  (is (= 0 (vm0::get-depth stack-frames 'a)))
  (is (= 1 (vm0::get-depth stack-frames 'b)))
  (is (= 2 (vm0::get-depth stack-frames 'c))))
