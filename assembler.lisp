(in-package :vm0)

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
