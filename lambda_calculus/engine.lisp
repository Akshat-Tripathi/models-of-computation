(load "core.lisp")

(defun is-redex(expr)
  (if (and (consp expr)
           (consp (car expr))
           (cdr expr))
    (eq (caar expr) 'lam)))

;; Evaluates a redex once, if the redex cannot be further evaluated, does nothing
(defun eval-1-redex(expr)
  (if (is-redex expr)
    (multiple-value-bind (arg exp) (from-lam (car expr))
                         (let ((reduced (sub exp arg (cadr expr)))
                               (not-done (cddr expr)))
                           (if not-done
                             (cons reduced not-done)
                             reduced)))
    expr))

(defun eval-redex(expr)
  (if (is-redex expr)
    (eval-redex (eval-1-redex expr))
    expr))

(defun normal-order(expr)
  (if (is-redex expr)
    (normal-order (eval-redex expr))
    (switch-expr expr
                 expr
                 (to-lam arg (normal-order exp))
                 (map 'list #'normal-order expr))))

(defparameter complex-expr
  '(((lam x \. lam y \. x y x) t u) ((lam x \. lam y \. lam z \. x ((lam x \. x x) y)) v ((lam x \. x y) w))))
