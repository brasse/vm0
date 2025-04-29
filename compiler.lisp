(in-package :vm0)

(define-condition compiler-error (error)
  ((message :initarg :message :reader compiler-error-message)
   (expr :initarg :expr :reader compiler-error-expr))
  (:report (lambda (c s)
             (format s "compiler error: ~A~%in: ~S"
                     (compiler-error-message c)
                     (compiler-error-expr c)))))

(defun compiler-error (message &optional expr)
  (error 'compiler-error :message message :expr expr))

(defun print-frame (f stream depth)
  (declare (ignore depth))
  (print-unreadable-object (f stream :type t)
    (format stream "offset=~A vars=~S next-var-index=~A"
            (frame-offset f)
            (loop for k being the hash-keys of (frame-vars f)
                  collect (list k (gethash k (frame-vars f))))
            (frame-next-var-index f))))

(defstruct (frame (:constructor %make-frame) (:print-function print-frame))
  vars
  offset
  next-var-index)

(defun make-frame (&key (offset 0) (start-var-index 0) (vars '()))
  (let
      ((frame
         (%make-frame :vars (make-hash-table) :offset offset :next-var-index start-var-index)))
    (loop for var in vars do
      (frame-add-binding frame var))
    frame))

(defun frame-add-binding (frame var)
  (let ((vars (frame-vars frame)))
    (if (gethash var vars)
        (compiler-error (format nil "variable ~A already used" var))
        (progn
          (setf (gethash var vars) (frame-next-var-index frame))
          (incf (frame-next-var-index frame))))))

(defun get-depth (stack-frames var)
  (when (null stack-frames)
    (compiler-error (format nil "variable ~A not found" var)))
  (let ((var-offset (gethash var (frame-vars (car stack-frames)))))
    (if var-offset
        (+ var-offset (frame-offset (car stack-frames)))
        (get-depth (cdr stack-frames) var))))

(defstruct (function-info)
  name
  arity
  ast
  code)

(defun genkey (prefix)
  (intern (format nil "~A" (gensym prefix)) :keyword))

(defun function-label (name)
  (intern (format nil "fn-~A" name) :keyword))

(defun repeat-instructions (n instructions)
  (loop repeat n append instructions))

(defun to-keyword (symbol)
  (cond
    ((keywordp symbol) symbol)
    ((symbolp symbol) (intern (symbol-name symbol) :keyword))
    (t (compiler-error (format nil "don't know how to turn ~S into a keyword" symbol)))))

(defparameter *special-forms* '(fn progn let set if while not - + * / % = < <= > >= print))

(defun expr-type (expr)
  (cond
    ((numberp expr) :number)
    ((symbolp expr) :symbol)
    ((listp expr)
     (let ((head (car expr)))
       (cond ((member head *special-forms*) (to-keyword head))
             ((symbolp head) :call)
             (t (compiler-error (format nil "unknown expression head: ~A" head) expr)))))
    (t (compiler-error "malformed expression" expr))))

(defun ensure-one-value (old-offset new-offset expr context)
  (unless (= new-offset (1+ old-offset))
    (compiler-error (format nil "~A must push exactly one value" context) expr)))

(defun compile-body (body stack-frames offset tailp &optional (context "body") (context-expr nil))
  (unless (> (length body) 0)
    (compiler-error (format nil "~A must have at least one expression" context)
                    (or context-expr body)))
  (let ((body-code '()))
    (loop for expr in (butlast body) do
      (multiple-value-bind (code new-offset)
          (compile-expr expr stack-frames offset)
        (push code body-code)
        (push (repeat-instructions (- new-offset offset) '((:pop))) body-code)))
    (multiple-value-bind (final-code final-offset)
        (compile-expr (car (last body)) stack-frames offset tailp)
      (ensure-one-value offset final-offset (car (last body)) "last expr in body")
      (push final-code body-code)
      (values (apply #'append (nreverse body-code)) final-offset))))

(defun compile-let-bindings (bindings stack-frames offset)
  (let ((init-code '())
        (current-offset offset))
    (loop for binding in bindings do
      (unless (and (listp binding)
                   (= (length binding) 2)
                   (symbolp (car binding)))
        (compiler-error "let binding must be (symbol expr)" binding))
      (destructuring-bind (var expr-a) binding
        (multiple-value-bind (code-a offset-a)
            (compile-expr expr-a stack-frames current-offset)
          (ensure-one-value current-offset offset-a expr-a "value of let bind")
          (push code-a init-code)
          (frame-add-binding (car stack-frames) var)
          (setf current-offset offset-a))))
    (values (apply #'append (nreverse init-code)) current-offset)))

(defun compile-let (expr stack-frames offset)
  (destructuring-bind (bindings . body) (cdr expr)
    (push (make-frame :offset offset) stack-frames)
    (multiple-value-bind (init-code init-offset)
        (compile-let-bindings bindings stack-frames offset)
      (multiple-value-bind (body-code final-offset)
          (compile-body body stack-frames init-offset nil "let body" expr)
        (ensure-one-value init-offset final-offset body "let body")
        (pop stack-frames)
        (values(append init-code
                       body-code
                       ;; pop all locals
                       (repeat-instructions
                        (- init-offset offset) '((:push 1) (:roll) (:pop))))
               (1+ offset))))))

(defun compile-fn (expr)
  (destructuring-bind (name args . body) (cdr expr)
    (let ((n-args (length args))
          (offset 2)) ;; offset is 2 here because fp and pc is on the stack above current fp
      (multiple-value-bind (code new-offset)
          (compile-body body (list (make-frame :start-var-index (- (1+ n-args)) :vars args)) offset t
                        (format nil "function body of ~A" name) expr)
        (ensure-one-value offset new-offset body "function body")
        (values (append
                 `((:label ,(function-label name)))
                 code
                 ;; stack: [ x arg n-args fp pc ret-v ]
                 ;; bring up fp and pc to be able to :ret
                 '((:push 2) (:roll)
                   (:push 2) (:roll)
                   (:ret))))))))

(defun compile-arg-list (arg-list stack-frames offset)
  (let ((arg-offset offset))
    (values
     (loop for arg-expr in arg-list
           append (multiple-value-bind (arg-code new-offset)
                      (compile-expr arg-expr stack-frames arg-offset)
                    (ensure-one-value arg-offset new-offset arg-expr "function argument")
                    (incf arg-offset)
                    arg-code))
     (+ offset (length arg-list)))))

(defun normal-call (name arg-list-code n-args)
  (let ((pop-start (genkey "pop-start-")) (pop-end (genkey "pop-end-")))
    (append arg-list-code
            `((:push ,n-args)
              (:call ,(function-label name))

              ;; stack: [ x arg1 arg2 n-args v ]
              (:push 1) (:roll)

              ;; stack: [ x arg1 arg2 v n-args ]
              ;; pop all args
              (:label ,pop-start)
              (:dup)
              (:jz ,pop-end)
              (:push 2) (:roll) (:pop) (:dec)
              (:jmp ,pop-start)
              (:label ,pop-end)

              ;; stack: [ x v 0 ]
              (:pop)))))

(defun tail-call (name arg-list-code n-args)
  (let ((pop-old-start (genkey "pop-old-start-")) (pop-old-end (genkey "pop-old-end-")))
    (append arg-list-code
            `(;; stack: [ x old-arg1 old-arg2 old-n-args fp pc new-arg1 ]
              (:push ,(+ n-args 2)) (:roll) (:dup) ;; we need an extra copy later

              ;; stack: [ x old-arg1 old-arg2 fp pc new-arg1 old-n-args old-n-args]
              ;; pop all old args
              (:label ,pop-old-start)
              (:dup)
              (:jz ,pop-old-end)
              (:push ,n-args) (:push 4) (:add) (:roll) (:pop)
              (:dec)
              (:jmp ,pop-old-start)
              (:label ,pop-old-end)

              ;; stack: [ x fp pc new-arg1 old-n-args 0 ]
              ;; clean up counter
              (:pop)

              ;; stack: [ x fp pc new-arg1 old-n-args ]
              ;; bring up fp and pc above new args
              (:push ,(+ n-args 2)) (:roll)
              (:push ,(+ n-args 2)) (:roll)

              ;; stack: [ x new-arg1 old-n-args fp pc ]
              ;; bring up old-n-args
              (:push 2) (:roll)

              ;; stack: [ x new-arg1 fp pc old-n-args ]
              ;; get current fp and swap to prepare for sub
              (:getfp) (:swap)

              ;; stack: [ x new-arg1 fp pc current-fp old-n-args ]
              ;; calculate new fp
              (:sub) (:push ,n-args) (:add)

              ;; stack: [ x new-arg1 fp pc (current-fp - old-n-args + new-n-args) ]
              (:setfp)

              ;; stack: [ x new-arg1 fp pc ]
              ;; push new-n-args and put it below fp and pc
              (:push ,n-args)
              (:push 2) (:roll)
              (:push 2) (:roll)

              ;; stack: [x new-arg1 new-n-arg fp pc ]
              (:jmp ,(function-label name))))))

(defun compile-call (expr stack-frames offset tailp)
  (declare (special function-table))
  (destructuring-bind (name . arg-list) expr
    (let ((function-info (gethash name function-table)) (n-args (length arg-list)))
      (unless function-info (compiler-error (format nil "unknown function: ~A" name)))
      (let ((arity (function-info-arity function-info)))
        (unless (= arity n-args)
          (compiler-error
           (format nil "function ~A called with ~A args, expects ~A" name n-args arity)))
        (multiple-value-bind (arg-list-code arg-offset) (compile-arg-list arg-list stack-frames offset)
          (declare (ignore arg-offset))
          (values
           (if tailp
               (tail-call name arg-list-code n-args)
               (normal-call name arg-list-code n-args))
           (1+ offset)))))))

(defun compile-unop (expr instruction stack-frames offset)
  (let ((expr-a (cadr expr)))
    (multiple-value-bind (code-a offset-a) (compile-expr expr-a stack-frames offset nil)
      (ensure-one-value offset offset-a expr-a (format nil "argument of ~A" (car expr)))
      (values (append code-a `((,instruction))) (1+ offset)))))

(defun compile-binop (expr instruction stack-frames offset)
  (destructuring-bind (expr-a expr-b) (cdr expr)
    (multiple-value-bind (code-a offset-a) (compile-expr expr-a stack-frames offset nil)
      (ensure-one-value offset offset-a expr-a (format nil "LHS of ~A" (car expr)))
      (multiple-value-bind (code-b offset-b) (compile-expr expr-b stack-frames offset-a nil)
        (ensure-one-value offset-a offset-b expr-b (format nil "RHS of ~A" (car expr)))
        (values (append code-a code-b `((,instruction))) (1+ offset))))))

(defun compile-if (expr stack-frames offset tailp)
  (unless (= (length (cdr expr)) 3)
    (compiler-error "if must have exactly three aruments" expr))
  (destructuring-bind (expr-cond expr-a expr-b) (cdr expr)
    (multiple-value-bind (code-cond offset-cond)
        (compile-expr expr-cond stack-frames offset nil)
      (ensure-one-value offset offset-cond expr-cond "if condition")

      (multiple-value-bind (code-true offset-true)
          (compile-expr expr-a stack-frames offset tailp)

        (multiple-value-bind (code-false offset-false)
            (compile-expr expr-b stack-frames offset tailp)
          (ensure-one-value offset offset-true expr-a "if true branch")
          (ensure-one-value offset offset-false expr-b "if false branch")
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
  (unless (>= (length (cdr expr)) 2)
    (compiler-error "while must have at least two arguments" expr))
  (destructuring-bind (expr-cond . body) (cdr expr)
    (multiple-value-bind (code-cond offset-cond)
        (compile-expr expr-cond stack-frames offset nil)
      (ensure-one-value offset offset-cond expr-cond "while condition")
      (let ((label-start (genkey "start-")) (label-end (genkey "end-")))
        (multiple-value-bind (code-body body-offset)
            (compile-body body stack-frames offset nil)
          (values (append
                   `((:label ,label-start))
                   code-cond
                   `((:jz ,label-end))
                   code-body
                   (repeat-instructions (- body-offset offset) '((:pop)))
                   `((:jmp ,label-start) (:label ,label-end)))
                  offset))))))

(defmacro unop (keyword instruction)
  `(;; case label
    ,keyword
    ;; compilation logic
    (compile-unop expr ,instruction stack-frames offset)))

(defmacro binop (keyword instruction)
  `(;; case label
    ,keyword
    ;; compilation logic
    (compile-binop expr ,instruction stack-frames offset)))

(defmacro deflang-compile-expr (&rest rules)
  `(defun compile-expr (expr &optional (stack-frames '()) (offset 0) (tailp nil))
     (case (expr-type expr)
       ,@(loop for rule in rules
               collect (macroexpand rule))
       (t (compiler-error "unknown expression" expr)))))

(deflang-compile-expr
  (:number (values `((:push ,expr)) (1+ offset)))

  (:symbol (values `((:push ,(get-depth stack-frames expr)) (:pick)) (1+ offset)))

  (:print
   (unless (cdr expr)
     (compiler-error "print requires exactly one value" expr))
   (multiple-value-bind (code new-offset) (compile-expr (cadr expr) stack-frames offset nil)
     (ensure-one-value offset new-offset (cadr expr) "argument of print")
     (values (append code '((:print))) offset)))

  (:set
   (unless (and (= (length expr) 3)
                (symbolp (cadr expr)))
     (compiler-error "set must be (set symbol expr)" expr))
   (destructuring-bind (var expr-a) (cdr expr)
     (multiple-value-bind (code-a offset-a) (compile-expr expr-a stack-frames offset nil)
       (ensure-one-value offset offset-a expr-a "argument of set")
       (values (append code-a
                       `((:push ,(get-depth stack-frames var)) (:set)))
               offset))))

  (:let (compile-let expr stack-frames offset))

  (:if (compile-if expr stack-frames offset tailp))

  (:while (compile-while expr stack-frames offset))

  (:progn
    (unless (> (length (cdr expr)) 0)
      (compiler-error "body of progn must have at least one exprssion"))
    (multiple-value-bind (code new-offset) (compile-body (cdr expr) stack-frames offset tailp)
      (ensure-one-value offset new-offset expr "progn")
      (values code (1+ offset))))

  (:fn (values '() offset))

  (:call (compile-call expr stack-frames offset tailp))

  (:- (destructuring-bind (a &optional b) (cdr expr)
        (if b
            (compile-binop expr :sub stack-frames offset)
            (multiple-value-bind (code-a new-offset)
                (compile-expr a stack-frames (1+ offset) nil)
              ;; TODO: consider binop and unop macros to work with instruction templates
              ;;       perpaps when this pattern show up again
              (unless (= new-offset (+ offset 2)) "RHS of - must push exactly one value" a)
              (values (append '((:push 0))
                              code-a
                              '((:sub)))
                      (1+ offset))))))
  (unop :not :not)

  (binop :+ :add)
  (binop :* :mul)
  (binop :/ :div)
  (binop :% :mod)

  (binop := :eq)
  (binop :< :lt)
  (binop :<= :lte)
  (binop :> :gt)
  (binop :>= :gte))

(defun collect-functions-fn (function-table)
  (lambda (ast)
    (when (and (listp ast) (eq (car-safe ast) 'fn))
      (let ((name (cadr ast))
            (args (caddr ast)))
        (unless (symbolp name)
          (compiler-error (format nil "function name must be a symbol: ~A" name)))
        (unless (and (listp args) (every #'symbolp args))
          (compiler-error
           (format nil "function arguments for ~A must be a list of symbols: ~A" name args)))
        (unless (= (length args) (length (remove-duplicates args)))
          (compiler-error (format nil "duplicate arguments in function ~A: ~A" name args)))
        (setf (gethash name function-table)
              (make-function-info :name name :arity (length args) :ast ast))))
    ast))

(defun compile-functions (function-table)
  (loop for function-info being the hash-values in function-table
        do (setf (function-info-code function-info)
                 (compile-fn (function-info-ast function-info))))
  function-table)

(defun compile-program (program)
  (let ((function-table (make-hash-table)))
    (declare (special function-table))
    (walk-ast program (collect-functions-fn function-table))
    (compile-functions function-table)
    (append
     (loop for expr in program
           append (multiple-value-bind (code offset) (compile-expr expr '())
                    (assert (zerop offset) () "expression in program must clean up stack")
                    code))
     '((:halt))
     (loop for function-info being the hash-values in function-table
           append (function-info-code function-info)))))
