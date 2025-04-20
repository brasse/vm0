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

(defun genkey (prefix)
  (intern (format nil "~A" (gensym prefix)) :keyword))

(defun clean-up-stack (n)
  (make-list n :initial-element '(:pop)))

(defun expr-type (expr)
  (cond
    ((numberp expr) :number)
    ((symbolp expr) :symbol)
    ((listp expr)
     (let ((op (car expr)))
       (cond
         ;; If already a keyword, use it as-is
         ((keywordp op) op)
         ;; If a normal symbol, intern its name into :keyword
         ((symbolp op) (intern (symbol-name op) :keyword))
         ;; Otherwise, error
         (t (error "Expression with invalid operator: ~S" expr)))))
    (t (error "Malformed expression: ~S" expr))))

(defun compile-body (body stack-frames offset)
  (let ((body-code '()))
    (loop for expr in (butlast body) do
      (multiple-value-bind (code new-offset)
          (compile-expr expr stack-frames offset)
        (push code body-code)
        (push (clean-up-stack (- new-offset offset)) body-code)))
    (multiple-value-bind (final-code final-offset)
        (compile-expr (car (last body)) stack-frames offset)
      (push final-code body-code)
      (values (apply #'append (nreverse body-code)) final-offset))))

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

(defun compile-let (expr stack-frames offset)
  (destructuring-bind (bindings . body) (cdr expr)
    (push (make-frame offset) stack-frames)
    (multiple-value-bind (init-code init-offset)
        (compile-let-bindings bindings stack-frames offset)
      (multiple-value-bind (body-code final-offset)
          (compile-body body stack-frames init-offset)
        (pop stack-frames)
        (values(append init-code
                       body-code
                       (clean-up-stack (- final-offset offset)))
               offset)))))

(defun compile-binop (expr instruction stack-frames offset)
  (destructuring-bind (expr-a expr-b) (cdr expr)
    (multiple-value-bind (code-a offset-a) (compile-expr expr-a stack-frames offset)
      (assert (= offset-a (1+ offset)) () "LHS must push exactly one value")
      (multiple-value-bind (code-b offset-b) (compile-expr expr-b stack-frames offset-a)
        (assert (= offset-b (1+ offset-a)) () "RHS must push exactly one value")
        (values (append code-a code-b `((,instruction))) (1+ offset))))))

(defun compile-if (expr stack-frames offset)
  (destructuring-bind (expr-cond expr-a expr-b) (cdr expr)
    (multiple-value-bind (code-cond offset-cond)
        (compile-expr expr-cond stack-frames offset)
      (assert (= offset-cond (1+ offset)) () "if condition must push exactly one value")

      (multiple-value-bind (code-true offset-true)
          (compile-expr expr-a stack-frames offset)

        (multiple-value-bind (code-false offset-false)
            (compile-expr expr-b stack-frames offset)
          (assert (= offset-true offset-false (1+ offset)) ()
                  "both if branches must push exactly one values")
          (let ((label-else (genkey "else-")) (label-end (genkey "end-")))
            (values (append
                     code-cond
                     `((:jz ,label-else))
                     code-true
                     `((:jmp ,label-end) (:label ,label-else))
                     code-false
                     `((:label ,label-end)))
                    offset-true)))))))

(defun compile-while (expr stack-frames offset)
  (destructuring-bind (expr-cond . body) (cdr expr)
    (multiple-value-bind (code-cond offset-cond)
        (compile-expr expr-cond stack-frames offset)
      (assert (= offset-cond (1+ offset)) () "while condition must push exactly one value")
      (let ((label-start (genkey "start-")) (label-end (genkey "end-")))
        (multiple-value-bind (code-body body-offset)
            (compile-body body stack-frames offset)
          (values (append
                   `((:label ,label-start))
                   code-cond
                   `((:jz ,label-end))
                   code-body
                   (clean-up-stack (- body-offset offset))
                   `((:jmp ,label-start) (:label ,label-end)))
                  offset))))))

(defmacro binop (keyword instruction)
  `(;; case label
    ,keyword
    ;; compilation logic
    (compile-binop expr ,instruction stack-frames offset)))

(defmacro deflang-compile-expr (&rest rules)
  `(defun compile-expr (expr stack-frames &optional (offset 0))
     (case (expr-type expr)
       ,@(loop for rule in rules
               collect (macroexpand rule))
       (t (error "unknown expression: ~S" expr)))))

(deflang-compile-expr
    (:number (values `((:push ,expr)) (1+ offset)))

    (:symbol (values `((:push ,(get-depth stack-frames expr)) (:pick)) (1+ offset)))

  (:print
   (multiple-value-bind (code new-offset) (compile-expr (cadr expr) stack-frames offset)
     (assert (= new-offset (1+ offset)) () "RHS of print must push exactly one value")
     (values (append code '((:print))) offset)))

  (:set
   (destructuring-bind (var expr-a) (cdr expr)
     (multiple-value-bind (code-a offset-a) (compile-expr expr-a stack-frames offset)
       (assert (= offset-a (1+ offset)) () "RHS of set must push exactly one value")
       (values (append code-a
                       `((:push ,(get-depth stack-frames var)) (:set)))
               offset))))

  (:let (compile-let expr stack-frames offset))

  (:if (compile-if expr stack-frames offset))

  (:while (compile-while expr stack-frames offset))

  (binop :+ :add)
  (binop :- :sub)
  (binop :* :mul)
  (binop :/ :div)
  (binop :% :mod)

  (binop := :eq)
  (binop :< :lt)
  (binop :> :gt))
