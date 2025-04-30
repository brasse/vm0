(in-package :vm0)

(defun walk-program (program &optional (node-fn #'identity))
  (mapcar (lambda (expr)
            (walk-ast expr node-fn))
          program))

(defun walk-ast (node &optional (node-fn #'identity))
  (cond
    ((atom node)
     (funcall node-fn node))

    ((null node)
     (funcall node-fn node))

    ((consp node)
     (let* ((head (car node))
            (args (cdr node))
            (new-args (mapcar (lambda (arg) (walk-ast arg node-fn)) args))
            (new-node (cons head new-args)))
       (funcall node-fn new-node)))

    (t (error "Unknown node type in AST: ~S" node))))
