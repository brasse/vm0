(in-package :vm0)

(defmacro defvm-instructions (&rest instructions)
  `(defun execute (instruction stack sp fp pc)
     (let ((control '(:continue)))
       (macrolet
           ((%validate-index (index mode &body body)
              `(let ((stack-pos (if (eq ,mode :bottom-up) (+ fp ,index) (- sp 1 ,index))))
                 (if (or (< stack-pos 0) (>= stack-pos (length stack)))
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
            (safe-read (depth &key (mode :bottom-up))
              `(%validate-index ,depth ,mode
                                (aref stack stack-pos)))
            (safe-write (depth value &key (mode :bottom-up))
              `(%validate-index ,depth ,mode
                                (setf (aref stack stack-pos) ,value)))
            (shift-down (depth)
              `(%validate-index ,depth :top-down
                                (loop
                                  for idx from (1+ stack-pos) to (1- sp)
                                  do (setf (aref stack (1- idx)) (aref stack idx)))))
            (pop-1 (a &body body)
              `(let ((,a (safe-pop)))
                 ,@body))
            (pop-2 (a b &body body)
              `(let ((,b (safe-pop)) (,a (safe-pop)))
                 ,@body))
            (pop-3 (a b c &body body)
              `(let ((,c (safe-pop)) (,b (safe-pop)) (,a (safe-pop)))
                 ,@body))
            (jump (target)
              `(return-from execute (values (list :jump ,target) sp fp)))
            (trap (reason)
              `(return-from execute (values (list :trap ,reason) sp fp)))
            (done ()
              `(return-from execute (values '(:done) sp fp))))

         (case (car instruction)
           ,@(loop for (name . body) in instructions
                   collect `(,name ,@body))
           (t (setf control `(:trap :unknown-instruction ,(car instruction))))))

       (values control sp fp))))

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
