(defpackage :vm0
  (:use :cl))

(in-package :vm0)

(defmacro defvm-instructions (&rest instructions)
  `(defun execute (instruction stack sp)
     (let ((control '(:continue)))
       (macrolet
           ((safe-pop ()
              `(if (<= sp 0)
                   (trap :stack-underflow)
                   (aref stack (decf sp))))
            (safe-push (val)
              `(if (>= sp (length stack))
                   (trap :stack-overflow)
                   (progn (setf (aref stack sp) ,val) (incf sp))))
            (safe-read (depth)
              `(let ((stack-pos (- sp 1 ,depth)))
                 (if (< stack-pos 0)
                     (trap :stack-out-of-bounds)
                     (aref stack stack-pos))))
            (safe-write (depth value)
              `(let ((stack-pos (- sp 1 ,depth)))
                 (if (< stack-pos 0)
                     (trap :stack-out-of-bounds)
                     (setf (aref stack stack-pos) ,value))))
            (shift-down (depth)
              `(let ((dst (- sp 1 ,depth)))
                 (if (< dst 0)
                     (trap :stack-out-of-bounds)
                     (loop
                       for idx from (1+ dst) to (1- sp)
                       do (setf (aref stack (1- idx)) (aref stack idx))))))
            (with-1 (a &body body)
              `(let ((,a (safe-pop)))
                 ,@body))
            (with-2 (a b &body body)
              `(let ((,b (safe-pop)) (,a (safe-pop)))
                 ,@body))
            (with-3 (a b c &body body)
              `(let ((,c (safe-pop)) (,b (safe-pop)) (,a (safe-pop)))
                 ,@body))
            (jump (target)
              `(return-from execute (values (list :jump ,target) sp)))
            (trap (reason)
              `(return-from execute (values (list :trap ,reason) sp)))
            (done ()
              `(return-from execute (values '(:done) sp))))

         (case (car instruction)
           ,@(loop for (name . body) in instructions
                   collect `(,name ,@body))
           (t (setf control `(:trap :unknown-instruction ,(car instruction))))))

       (values control sp))))

(defvm-instructions
    (:pop (safe-pop))
    (:push (safe-push (cadr instruction)))
  (:pick (with-1 depth (safe-push (safe-read depth))))
  (:roll (with-1 depth (let ((new-tos (safe-read depth)))
                         (shift-down depth)
                         (safe-write 0 new-tos))))
  (:dup (with-1 a (safe-push a) (safe-push a)))
  (:over (with-2 a b (safe-push b) (safe-push a) (safe-push b)))
  (:swap (with-2 a b (safe-push b) (safe-push a)))
  (:rot (with-3 a b c (safe-push c)  (safe-push a) (safe-push b)))
  (:add (with-2 a b (safe-push (+ a b))))
  (:sub (with-2 a b (safe-push (- a b))))
  (:inc (with-1 a (safe-push (1+ a))))
  (:dec (with-1 a (safe-push (1- a))))
  (:mul (with-2 a b (safe-push (* a b))))
  (:div (with-2 a b (if (zerop b)
                        (trap :divide-by-zero)
                        (safe-push (truncate a b)))))
  (:mod (with-2 a b (if (zerop b)
                        (trap :divide-by-zero)
                        (safe-push (nth-value 1 (truncate a b))))))
  (:eq (with-2 a b (safe-push (if (= a b) 1 0))))
  (:lt (with-2 a b (safe-push (if (< a b) 1 0))))
  (:gt (with-2 a b (safe-push (if (> a b) 1 0))))
  (:jmp (jump (cadr instruction)))
  (:jz (with-1 a (when (zerop a) (jump (cadr instruction)))))
  (:jnz (with-1 a (unless (zerop a) (jump (cadr instruction)))))
  (:print (with-1 a (format t "~A~%" a)))
  (:halt (done)))

(defun run (program &key (stack (make-array 1024 :initial-element 0)) trace)
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
        (setf sp new-sp)
        (when trace (format t "~S~%" (subseq stack 0 sp)))))))

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
        (:push 10)
        (:push 20)
        (:push 30)
        (:push 2)
        (:roll)
        (:print)
        )))
 :trace t)
