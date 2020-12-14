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

;;Finds all the free variables in an expression
(defun fv(expr)
  (match expr
         ((list* 'λ exp) (multiple-value-bind (args e) (split-dot exp)
                                         (set-difference (fv e) args)))
         ((guard exp (= 1 (length exp))) exp)
         (_ expr)))


(defparameter true '(λ x y \. x))
(defparameter v '(x))


(print (fv '(x y z)))
