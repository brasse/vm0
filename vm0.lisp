(defpackage :vm0
  (:use :cl))

(in-package :vm0)

(defparameter *stack* `())

(defun vm0-push (i)
  (push i *stack*)
  (format t "push ~A~%" *stack*))

(defun vm0-pop ()
  (let ((top (pop *stack*)))
    (format t "pop ~A~%" *stack*)
    top))

(defun execute (instruction)
  (case (car instruction)
    (:push (vm0-push (cadr instruction)))
    (:pop (vm0-pop))
    (:dup (let ((a (vm0-pop))) (vm0-push a) (vm0-push a)))
    (:add (vm0-push (+ (vm0-pop) (vm0-pop))))
    (:sub (let ((b (vm0-pop)) (a (vm0-pop))) (vm0-push (- a b))))
    (:mul (vm0-push (* (vm0-pop) (vm0-pop))))
    ;; div
    ;; mod
    (:eq (vm0-push (if (= (vm0-pop) (vm0-pop)) 1 0)))
    (:lt (let ((b (vm0-pop)) (a (vm0-pop))) (vm0-push (if (< a b) 1 0))))
    (:gt (let ((b (vm0-pop)) (a (vm0-pop))) (vm0-push (if (> a b) 1 0))))
    ;; jz
    ;; jnz
    (:print (format t "~A~%" (vm0-pop)))
    (:halt :done)
    (otherwise (format t "unknown inctructino~%"))))

(defun run (program)
  (let ((pc 0))
    (loop while (< pc (length program)) do
      (execute (elt program pc))
      (incf pc))))

(run '(
       (:push 40)
       (:push 2)
       (:add)
       (:push 20)
       (:sub)
       (:print)
       ))
