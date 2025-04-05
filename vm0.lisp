(defpackage :vm0
  (:use :cl))

(in-package :vm0)


(defun execute (instruction stack sp)
  (labels ((vm0-push (value) (setf (aref stack sp) value) (incf sp))
           (vm0-pop () (aref stack (decf sp))))
    (let ((control-directive '(:continue)))
      (case (car instruction)
        (:push (vm0-push (cadr instruction)))
        (:pop (vm0-pop))
        (:dup (let ((a (vm0-pop)))
                (vm0-push a) (vm0-push a)))
        (:over (let ((a (vm0-pop)) (b (vm0-pop)))
                 (vm0-push b) (vm0-push a) (vm0-push b)))
        (:swap (let ((a (vm0-pop)) (b (vm0-pop)))
                 (vm0-push a) (vm0-push b)))
        (:rot (let ((a (vm0-pop)) (b (vm0-pop)) (c (vm0-pop)))
                (vm0-push b) (vm0-push a) (vm0-push c)))
        (:add (vm0-push (+ (vm0-pop) (vm0-pop))))
        (:sub (let ((b (vm0-pop)) (a (vm0-pop)))
                (vm0-push (- a b))))
        (:inc (let ((a (vm0-pop)))
                (vm0-push (1+ a))))
        (:dec (let ((a (vm0-pop)))
                (vm0-push (1- a))))
        (:mul (vm0-push (* (vm0-pop) (vm0-pop))))
        (:div (let ((b (vm0-pop)) (a (vm0-pop)))
                (if (zerop b)
                    (setf control-directive '(:trap :divide-by-zero))
                    (vm0-push (truncate a b)))))
        (:mod (let ((b (vm0-pop)) (a (vm0-pop)))
                (if (zerop b)
                    (setf control-directive '(:trap :divide-by-zero))
                    (vm0-push (nth-value 1 (truncate a b))))))
        (:eq (vm0-push (if (= (vm0-pop) (vm0-pop)) 1 0)))
        (:lt (let ((b (vm0-pop)) (a (vm0-pop)))
               (vm0-push (if (< a b) 1 0))))
        (:gt (let ((b (vm0-pop)) (a (vm0-pop)))
               (vm0-push (if (> a b) 1 0))))
        (:jmp (setf control-directive `(:jump ,(cadr instruction))))
        (:jz (when (zerop (vm0-pop))
               (setf control-directive `(:jump ,(cadr instruction)))))
        (:jnz (when (not (zerop (vm0-pop)))
                (setf control-directive `(:jump ,(cadr instruction)))))
        (:print (format t "~A~%" (vm0-pop)))
        (:halt (setf control-directive '(:done)))
        (otherwise (setf control-directive `(:trap :unknown-instruction ,(car instruction)))))
      (values control-directive sp))))

(defun run (program &optional (stack (make-array 1024 :initial-element 0)))
  (let ((pc 0) (sp 0))
    (loop while (< pc (length program)) do
      (multiple-value-bind (control-directive new-sp) (execute (aref program pc) stack sp)
        (case (car control-directive)
          (:continue (incf pc))
          (:jump (setf pc (cadr control-directive)))
          (:trap
           (format t "trap: ~S~%" (cdr control-directive))
           (return))
          (:done (return))
          (otherwise (error "unknown control directive: ~S" (car control-directive))))
        (setf sp new-sp)))))

(defun assemble (program)
  (let ((program (if (vectorp program) program (coerce program 'vector)))
        (the-labels (make-hash-table)))
    ;; resolve label addresses
    (loop with addr = 0
          for instruction across program
          do (case (car instruction)
               (:label (setf (gethash (cadr instruction) the-labels) addr))
               (otherwise (incf addr))))
    ;; resolve jump labels
    (coerce
     (loop for instruction across program
           for x = (case (car instruction)
                     (:label nil)
                     ((:jmp :jz :jnz) (let ((target (gethash (cadr instruction) the-labels)))
                                        (unless target
                                          (error "undefined label: ~S" (cadr instruction)))
                                        (list (car instruction) target)))
                     (otherwise instruction))
           when x
             collect x)
     'vector)))

(defun syntax (program)
  (let ((program (if (vectorp program) program (coerce program 'vector))))
    (loop for instruction across program
          do (unless
                 (case (car instruction)
                   (:push (typep (cadr instruction) 'integer))
                   ((:jmp :jz :jnz :label) (typep (cadr instruction) 'keyword))
                   (otherwise (typep (car instruction) 'keyword)))
               (error "syntax error: ~S" instruction)))
    program))

;;; factorial
(run
 (assemble
     (syntax
      '((:push 1)                      ; product
        (:push 5)                      ; counter, factorial to compute

        (:label :loop)
        (:dup)                          ; copy counter for check
        (:push 1)
        (:eq)
        (:jnz :done)

        (:dup)                          ; copy counter
        (:rot)                          ; now: counter product counter
        (:mul)                          ; counter * product
        (:swap)                         ; product result under counter
        (:dec)                          ; dec counter
        (:jmp :loop)

        (:label :done)
        (:pop)                          ; discard counter
        (:print)
        (:halt)))))

(run
 (assemble
     (syntax
      '(
        (:push 1)
        (:push 2)
        (:push 3)
        (:print)
        (:print)
        (:print)
        ))))
