(in-package :vm0)

(defparameter +factorial-stak-tr+
  '((fn fact-0 (n acc)
     (if (= n 1)
         acc
         (fact-0 (- n 1) (* acc n))))
    (fn fact (n) (fact-0 n 1))
    (print (fact 1000))))

(defparameter +fibonacci-stak+
  '((fn fib (n)
     (if (<= n 2)
         1
         (+ (fib (- n 1)) (fib (- n 2)))))
    (print (fib 15))))

(defparameter +fibonacci-stak-tr+
  '((fn fib-0 (n a b)
     (if (= n 0)
         a
         (fib-0 (- n 1) b (+ a b))))
    (fn fib (n) (fib-0 n 0 1))
    (print (fib 500))))

(defparameter +even-odd-stak-tr+
  '((fn even (n)
     (if (= n 0)
         0
         (odd (- n 1))))
    (fn odd (n)
     (if (= n 0)
         1
         (even (- n 1))))
    (print (even 100001))))

(defparameter +count-down-stak-tr+
  '((fn count-down (n)
     (if (= n 0)
         0
         (count-down (- n 1))))
    (print (count-down 100000))))

(defparameter +counter-stak-tr+
  '((fn counter (current limit)
     (if (> current limit)
         current
         (progn
           (print current)
           (counter (+ current 1) limit))))
    (print (counter 0 20))))

(defparameter +does-not-break-horribly+
  ;; tail calls are not be optimized inside let
  ;; doing so is hard and we won't do it in this project
  '((fn tail (x) (* x 2))
    (fn outer (x) (let ((a (* x 2))) (tail a)))
    (print (outer 100))))
