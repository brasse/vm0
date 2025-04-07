(in-package :vm0)

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

(defun assemble-and-run (&key program trace)
  (run (assemble (macro (syntax program)))
       :trace trace))
