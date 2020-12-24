(load "core.lisp"  :external-format :utf-8)

(defun is-redex(expr)
  (if (and (consp expr)
           (consp (car expr))
           (cdr expr))
    (eq (caar expr) 'λ)))

;; Evaluates a redex once, if the redex cannot be further evaluated, does nothing
(defun eval-1-redex(expr)
  (if (is-redex expr)
    (multiple-value-bind (arg exp) (from-λ (car expr))
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

(defmacro try-reduce(expr func body)
  `(if (is-redex ,expr)
     (,func (eval-redex ,expr))
     ,body))

(defun normal-order(expr)
  (try-reduce expr
              normal-order
              (switch-expr expr
                           expr
                           (to-λ arg (normal-order exp))
                           (let ((mapped (map 'list #'normal-order expr)))
                             (try-reduce mapped normal-order mapped)))))

(defparameter complex-expr
  '(((λ x \. λ y \. x y x) t u) ((λ x \. λ y \. λ z \. x ((λ x \. x x) y)) v ((λ x \. x y) w))))
