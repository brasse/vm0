(in-package :vm0)

(defun run-program (program &key (stack (make-array 1024 :initial-element 0)) trace)
  (let ((pc 0) (sp 0) (fp 0))
    (loop while (< pc (length program)) do
      (let ((instruction (aref program pc)))
        (multiple-value-bind (control-directive new-sp new-fp)
            (let ((return-address (1+ pc)))
              (execute instruction stack sp fp return-address))
          (case (car control-directive)
            (:continue (incf pc))
            (:jump (setf pc (cadr control-directive)))
            (:trap
             (format t "trap: ~S~%" (cdr control-directive))
             (return))
            (:done (return))
            (otherwise (error "unknown control directive: ~S" (car control-directive))))
          (setf sp new-sp)
          (setf fp new-fp)
          (when trace (format t "~A ~A ~A~%" instruction fp (subseq stack 0 sp))))))))

(defun assemble-and-run (&key program trace)
  (run-program (assemble (macro (syntax program)))
               :trace trace))
