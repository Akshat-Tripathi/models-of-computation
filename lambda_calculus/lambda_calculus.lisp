(ql:quickload "trivia")
(use-package :trivia)

;; 3 types of expression
;; 1. Variables i.e. x
;; 2. Abstractions i.e. λ x . x
;; 3. Applications i.e. M M

(defun split-dot-helper(l start)
  (match l
         ((cons '\. rest) (values start rest))
         ((cons x y) (split-dot-helper y (cons x start)))))

(defun split-dot(l)
  (split-dot-helper l '()))

;;Use this to pattern match expressions
(defmacro switch-expr(expr abs var app)
  `(match ,expr
          ((list* 'λ exp) ,abs)
          ((guard exp (= 1 (length exp))) ,var)
          (_ ,app)))


;;Finds all the free variables in an expression
(defun fv(expr)
  (switch-expr expr
               (multiple-value-bind (args e) (split-dot exp)
                                    (set-difference (fv e) args))
               exp
               expr))


(defparameter true '(λ x y \. x))
(defparameter v '(x))
(print (fv '(x y z)))
