(in-package :vm0)

(defun print-frame (f stream depth)
  (declare (ignore depth))
  (print-unreadable-object (f stream :type t)
    (format stream "offset=~A vars=~S"
            (frame-offset f)
            (loop for k being the hash-keys of (frame-vars f)
                  collect (list k (gethash k (frame-vars f)))))))

(defstruct (frame (:constructor %make-frame) (:print-function print-frame))
  vars
  offset)

(defun make-frame (&optional (offset 0))
  (%make-frame :vars (make-hash-table) :offset offset))

(defun frame-add-binding (frame var)
  (let ((vars (frame-vars frame)))
    (if (gethash var vars)
        (error "binding ~S already used" var)
        (setf (gethash var vars) (hash-table-count vars)))))

(defun get-depth (stack-frames var)
  (when (null stack-frames)
    (error "binding ~S not found" var))
  (let ((var-offset (gethash var (frame-vars (car stack-frames)))))
    (if var-offset
        (+ var-offset (frame-offset (car stack-frames)))
        (get-depth (cdr stack-frames) var))))

(defun compile-let-bindings (bindings stack-frames offset)
  (let ((init-code '())
        (current-offset offset))
    (loop for binding in bindings do
      (destructuring-bind (var expr-a) binding
        (multiple-value-bind (code-a offset-a)
            (compile-expr expr-a stack-frames current-offset)
          (assert (= offset-a (1+ current-offset)) () "RHS of let must push exactly one value")
          (push code-a init-code)
          (frame-add-binding (car stack-frames) var)
          (setf current-offset offset-a))))
    (values (apply #'append (nreverse init-code)) current-offset)))

(defun compile-body (body stack-frames offset)
  (let ((body-code '()))
    (loop for expr in (butlast body) do
      (multiple-value-bind (code new-offset)
          (compile-expr expr stack-frames offset)
        (push code body-code)
        (push (make-list (- new-offset offset) :initial-element '((:pop))) body-code)))
    (multiple-value-bind (final-code final-offset)
        (compile-expr (car (last body)) stack-frames offset)
      (push final-code body-code)
      (values (apply #'append (nreverse body-code)) final-offset))))

(defun compile-expr (expr stack-frames &optional (offset 0))
  (cond
    ((numberp expr)
     (values `((:push ,expr)) (1+ offset)))
    
    ((symbolp expr)
     (values `((:push ,(get-depth stack-frames expr)) (:pick)) (1+ offset)))
    
    ((and (listp expr) (eq (car expr) 'set))
     (destructuring-bind (var expr-a) (cdr expr)
       (multiple-value-bind (code-a offset-a) (compile-expr expr-a stack-frames offset)
         (assert (= offset-a (1+ offset)) () "RHS of set must push exactly one value")
         (values (append code-a
                         `((:push ,(get-depth stack-frames var)) (:set)))
                 offset))))

    ((and (listp expr) (eq (car expr) 'let))
     (destructuring-bind (bindings . body) (cdr expr)
       (push (make-frame offset) stack-frames)
       (multiple-value-bind (init-code init-offset)
           (compile-let-bindings bindings stack-frames offset)
         (multiple-value-bind (body-code final-offset)
             (compile-body body stack-frames init-offset)
           (pop stack-frames)
           (values(append init-code
                          body-code
                          (make-list (- final-offset offset) :initial-element '((:pop))))
                  offset)))))

    ((and (listp expr) (eq (car expr) '+))
     (destructuring-bind (expr-a expr-b) (cdr expr)
       (multiple-value-bind (code-a offset-a) (compile-expr expr-a stack-frames offset)
         (assert (= offset-a (1+ offset)) () "LHS of + must push exactly one value")
         (multiple-value-bind (code-b offset-b) (compile-expr expr-b stack-frames offset-a)
           (assert (= offset-b (1+ offset-a)) () "RHS of + must push exactly one value")
           (values (append code-a code-b '((:add))) (1+ offset))))))

    ;; handle let, if, while here ...
    (t (error "unknown expression: ~S" expr))))
