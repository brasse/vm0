(defpackage :vm0
  (:use :cl))

(in-package :vm0)

(defparameter *stack* `())

(defun vm0-push (i)
  (push i *stack*))

(defun vm0-pop ()
  (pop *stack*))


(defun execute (instruction)
  (case (car instruction)
    (:push (vm0-push (cadr instruction)) '(:continue))
    (:pop (vm0-pop) '(:continue))
    (:dup (let ((a (vm0-pop))) (vm0-push a) (vm0-push a)) '(:continue))
    (:add (vm0-push (+ (vm0-pop) (vm0-pop))) '(:continue))
    (:sub (let ((b (vm0-pop)) (a (vm0-pop))) (vm0-push (- a b))) '(:continue))
    (:mul (vm0-push (* (vm0-pop) (vm0-pop))) '(:continue))
    ;; div
    ;; mod
    (:eq (vm0-push (if (= (vm0-pop) (vm0-pop)) 1 0)) '(:continue))
    (:lt (let ((b (vm0-pop)) (a (vm0-pop))) (vm0-push (if (< a b) 1 0))) '(:continue))
    (:gt (let ((b (vm0-pop)) (a (vm0-pop))) (vm0-push (if (> a b) 1 0))) '(:continue))
    (:jmp `(:jump ,(cadr instruction)))
    (:jz (if (zerop (vm0-pop)) (cons :jump (cadr instruction)) '(:continue)))
    (:jnz (if (not (zerop (vm0-pop))) (cons :jump (cadr instruction)) '(:continue)))
    (:print (format t "~A~%" (vm0-pop)) '(:continue))
    (:halt '(:done))
    (otherwise (error "unknown instruction: ~S" (car instruction)))))

(defun run (program)
  (let ((pc 0))
    (loop while (< pc (length program)) do
      (let ((control-directive (execute (elt program pc))))
        (case (car control-directive)
          (:continue (incf pc))
          (:jump (setf pc (cadr control-directive)))
          (:done (return))
          (otherwise (error "unknown control directive: ~S" (car control-directive))))))))

(run #(
       (:push 40)
       (:push 2)
       (:add)
       (:push 20)
       (:sub)
       (:print)
       (:jmp 9)
       (:push 100)
       (:print)
       (:push 200)
       (:print)
       (:halt)
       (:push 1337)
       (:print)
       ))
