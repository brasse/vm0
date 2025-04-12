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

(defun make-frame () (%make-frame :vars (make-hash-table) :offset 0))

(defun frame-size (frame)
  (+ (hash-table-count (frame-vars frame)) (frame-offset frame)))

(defun frame-inc-offset (frame)
  (incf (frame-offset frame)))

(defun frame-dec-offset (frame)
  (decf (frame-offset frame)))

(defun frame-add-binding (frame var)
  (let ((vars (frame-vars frame)))
    (if (gethash var vars)
        (error "binding ~S already used" var)
        (maphash (lambda (v d)
                   (setf (gethash v vars) (1+ d)))
                 vars))
    (setf (gethash var vars) 0)))

(defun frame-get-depth (frame var)
  (let ((var-depth (gethash var (frame-vars frame))))
    (if var-depth
        (+ var-depth (frame-offset frame))
        nil)))

(defun get-depth (stack-frames var &optional (total-offset 0))
  (let* ((top-frame (car stack-frames)) (depth (frame-get-depth top-frame var)))
    (if depth
        (+ total-offset depth)
        (get-depth (cdr stack-frames) var (+ total-offset (frame-size top-frame))))))

(defun compile-expr-0 (expr stack-frames &optional (offset 0))
  (cond
    ((numberp expr)
     (values `((:push ,expr)) 1))
    
    ((symbolp expr)
     (values `((:push ,(get-depth stack-frames expr)) (:pick)) 1))
    
    ;; ((and (listp expr) (eq (car expr) 'set))
    ;;  (destructuring-bind (var expr-a) (cdr expr)
    ;;    (let ((offset-a (frame-offset (car stack-frames)))
    ;;          (expr-a-code (compile-expr expr-a stack-frames)))
    ;;      (assert (= offset-a (1+ offset)) () "RHS of set must push exactly one value")
    ;;      (frame-dec-offset (car stack-frames))
    ;;      (append expr-a-code
    ;;              `((:push ,(get-depth stack-frames var)) (:set))))))

    ((and (listp expr) (eq (car expr) '+))
     (destructuring-bind (expr-a expr-b) (cdr expr)
       (multiple-value-bind (code-a offset-a) (compile-expr expr-a stack-frames)
         (multiple-value-bind (code-b offset-b) (compile-expr expr-b stack-frames)
           (assert (= offset-a 1) () "LHS of + must push exactly one value")
           (assert (= offset-b 1) () "RHS of + must push exactly one value")
           (values (append code-a code-b '((:add)))
                   1)))))

    ;; ((and (listp expr) (eq (car expr) 'let))
    ;;  (destructuring-bind (bindings . body) (cdr expr)
    ;;    (push (make-frame) stack-frames)
    ;;    (let ((init-code
    ;;            (loop for binding in bindings
    ;;                  collect
    ;;                  (destructuring-bind (var expr-a) binding
    ;;                    (let ((code (compile-expr expr-a stack-frames)))
    ;;                      (prog1
    ;;                          code 
    ;;                        (frame-add-binding (car stack-frames) var)
    ;;                        (frame-dec-offset (car stack-frames))))))))
    ;;      (prog1
    ;;          (append
    ;;           (apply #'append init-code)
    ;;           (compile-expr (car body) stack-frames)
    ;;           (make-list (frame-size (car stack-frames)) :initial-element '((:pop))))
    ;;        (pop stack-frames)))))

    ;; handle let, if, while here ...
    (t (error "unknown expression: ~S" expr))))

(defun compile-expr (expr stack-frames)
  (cond
    ((numberp expr)
     (frame-inc-offset (car stack-frames))
     `((:push ,expr)))
    
    ((symbolp expr)
     (prog1
         `((:push ,(get-depth stack-frames expr)) (:pick))
       (frame-inc-offset (car stack-frames))))
    
    ((and (listp expr) (eq (car expr) 'set))
     (destructuring-bind (var expr-a) (cdr expr)
       (let ((offset-a (frame-offset (car stack-frames)))
             (expr-a-code (compile-expr expr-a stack-frames)))
         (assert (= offset-a (1+ offset)) () "RHS of set must push exactly one value")
         (frame-dec-offset (car stack-frames))
         (append expr-a-code
                 `((:push ,(get-depth stack-frames var)) (:set))))))

    ((and (listp expr) (eq (car expr) '+))
     (destructuring-bind (expr-a expr-b) (cdr expr)
       (let* ((base-offset (frame-offset (car stack-frames)))
              (code-a (compile-expr expr-a stack-frames))
              (offset-a (frame-offset (car stack-frames))))
         (assert (= offset-a (1+ base-offset)) () "LHS of + must push exactly one value")
         (let* ((code-b (compile-expr expr-b stack-frames))
                (offset-b (frame-offset (car stack-frames))))
           (assert (= offset-b (1+ offset-a)) () "RHS of + must push exactly one value")
           (frame-dec-offset (car stack-frames))
           (append code-a code-b '((:add)))))))

    ((and (listp expr) (eq (car expr) 'let))
     (destructuring-bind (bindings . body) (cdr expr)
       (push (make-frame) stack-frames)
       (let ((init-code
               (loop for binding in bindings
                     collect
                     (destructuring-bind (var expr-a) binding
                       (let ((code (compile-expr expr-a stack-frames)))
                         (prog1
                             code 
                           (frame-add-binding (car stack-frames) var)
                           (frame-dec-offset (car stack-frames))))))))
         (prog1
             (append
              (apply #'append init-code)
              (compile-expr (car body) stack-frames)
              (make-list (frame-size (car stack-frames)) :initial-element '((:pop))))
           (pop stack-frames)))))

    ;; handle let, if, while here ...
    (t (error "unknown expression: ~S" expr))))
