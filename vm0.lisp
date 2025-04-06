(defpackage :vm0
  (:use :cl))

(in-package :vm0)

(defmacro defvm-instructions (&rest instructions)
  `(defun execute (instruction stack sp)
     (let ((control '(:continue)))
       (macrolet
           ((%validate-depth (depth &body body)
              `(let ((stack-pos (- sp 1 ,depth)))
                 (if (or (< ,depth 0) (< stack-pos 0))
                     (trap :stack-out-of-bounds)
                     ,@body)))
            (safe-pop ()
              `(if (<= sp 0)
                   (trap :stack-underflow)
                   (aref stack (decf sp))))
            (safe-push (val)
              `(if (>= sp (length stack))
                   (trap :stack-overflow)
                   (progn (setf (aref stack sp) ,val) (incf sp))))
            (safe-read (depth)
              `(%validate-depth ,depth
                                (aref stack stack-pos)))
            (safe-write (depth value)
              `(%validate-depth ,depth
                                (setf (aref stack stack-pos) ,value)))
            (shift-down (depth)
              `(%validate-depth ,depth
                                (loop
                                  for idx from (1+ stack-pos) to (1- sp)
                                  do (setf (aref stack (1- idx)) (aref stack idx)))))
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
  (:add (with-2 a b (safe-push (+ a b))))
  (:sub (with-2 a b (safe-push (- a b))))
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
      (let ((instruction (aref program pc)))
        (multiple-value-bind (control-directive new-sp) (execute instruction stack sp)
          (case (car control-directive)
            (:continue (incf pc))
            (:jump (setf pc (cadr control-directive)))
            (:trap
             (format t "trap: ~S~%" (cdr control-directive))
             (return))
            (:done (return))
            (otherwise (error "unknown control directive: ~S" (car control-directive))))
          (setf sp new-sp)
          (when trace (format t "~S ~S~%" instruction (subseq stack 0 sp))))))))

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

(defmacro defvm-macros (&rest macros)
  `(defun macro (program)
     (labels ((flatten (l)
                (cond ((null l) '())
                      ((every #'atom (car l)) (cons (car l) (flatten (cdr l))))
                      (t (concatenate 'list (flatten (car l)) (flatten (cdr l)))))))
       (let ((program (if (vectorp program) program (coerce program 'vector))))
         (coerce
          (flatten (loop for instruction across program
                         for x = (case (car instruction)
                                   ,@(loop for (name . body) in macros
                                           collect `(,name ,@body))
                                   (t instruction))
                         collect x))
          'vector)))))

(defvm-macros
    (:dup '((:push 0) (:pick)))
    (:over '((:push 1) (:pick)))
  (:swap '((:push 1) (:roll)))
  (:rot '((:push 2) (:roll)))
  (:inc '((:push 1) (:add)))
  (:dec '((:push 1) (:sub))))

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
     (macro
      (syntax
       '((:push 1)                     ; product
         (:push 5)                     ; counter, factorial to compute

         (:label :loop)
         (:dup)                         ; copy counter for check
         (:push 1)
         (:eq)
         (:jnz :done)

         (:dup)                         ; copy counter
         (:rot)                         ; now: counter product counter
         (:mul)                         ; counter * product
         (:swap)                        ; product result under counter
         (:dec)                         ; dec counter
         (:jmp :loop)

         (:label :done)
         (:pop)                         ; discard counter
         (:print)
         (:halt))))))

;;; fibonacci
(run
 (assemble
     (macro
      (syntax
       '((:push 0)                      ; a fib(n - 2)
         (:push 1)                      ; b fib(n - 1)
         (:push 10)                     ; n

         (:label :loop)
         (:dup)                         ; n
         (:push 0)
         (:eq)
         (:jnz :done)

         ;; next = a + b
         (:push 2) (:pick)              ; a
         (:push 2) (:pick)              ; b
         (:add)                         ; next

         ;; update a and b
         (:push 3) (:roll)              ; b → a
         (:push 2) (:roll)              ; next → b

         (:dec)                         ; n = n - 1

         ;; clean up old a
         (:swap)                        ; bring a to TOS
         (:pop)                         ; discard it

         (:jmp :loop)

         (:label :done)
         (:pop)                         ; discard n
         (:print)                       ; print b
         (:halt))))))
