(in-package :vm0/tests)

(def-suite stack-frames)

(in-suite stack-frames)

(defparameter f0 (vm0::make-frame))
(vm0::frame-add-binding f0 'a)
(vm0::frame-add-binding f0 'b)
(vm0::frame-add-binding f0 'c)
(vm0::frame-inc-offset f0)

(defparameter f1 (vm0::make-frame))
(vm0::frame-add-binding f1 'foo)
(vm0::frame-add-binding f1 'bar)
(vm0::frame-add-binding f1 'baz)
(vm0::frame-inc-offset f1)
(vm0::frame-inc-offset f1)

(defparameter stack-frames (list f1 f0))

(test frame-size-works
  (is (= 4 (vm0::frame-size f0)))
  (is (= 5 (vm0::frame-size f1))))

(test frame-get-depth-works
  (is (= 4 (vm0::frame-get-depth f1 'foo)))
  (is (= 3 (vm0::frame-get-depth f1 'bar)))
  (is (= 2 (vm0::frame-get-depth f1 'baz))))

(test get-depth-works
  (is (= 4 (vm0::get-depth-0 stack-frames 'foo)))
  (is (= 8 (vm0::get-depth-0 stack-frames 'a)))
  (is (= 7 (vm0::get-depth-0 stack-frames 'b)))
  (is (= 6 (vm0::get-depth-0 stack-frames 'c))))
