(in-package :vm0)

(defun walk-ast (expr &optional (node-fun #'identity))
  (cond
    ((or (numberp expr) (symbolp expr))
     (funcall node-fun expr))
    ((listp expr) (let ((new-expr (mapcar (lambda (subexpr)
                                            (funcall node-fun subexpr))
                                          expr)))
                    (funcall node-fun new-expr)))
    (t (compiler-error "malformed AST" expr))))


(defun car-safe (x)
  (if (and (listp x) (consp x))
      (car x)
      nil))
