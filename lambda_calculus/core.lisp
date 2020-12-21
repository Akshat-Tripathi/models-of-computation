(ql:quickload "trivia")
(use-package :trivia)

;; 3 types of expression
;; 1. Variables i.e. x
;; 2. Abstractions i.e. λ x . x
;; 3. Applications i.e. M M

(defun to-λ(arg expr)
  (cons 'λ (cons arg (cons '\. expr))))

(defun from-λ(λ)
  (values (cadr λ) (cdddr λ)))


;;Use this to pattern match expressions
(defmacro switch-expr(expr var abs app)
  `(match ,expr
           ((cons expr nil) ,var)
           ((cons 'λ _) (multiple-value-bind (arg exp) (from-λ ,expr) ,abs))
           ((cons _ _) ,app)
           (_ ,var)))

;;Finds all the free variables in an expression
(defun fv(expr)
  (let ((free (switch-expr expr
               expr
               (remove arg (fv exp))
               (reduce #'append (remove-duplicates (map 'list #'fv expr))))))
    (match free
           ((cons _ _) free)
           (_ (list free)))))

;;TODO alpha equivalence

;;Substitution
(defvar symbol-bank '(a b c d e f g h i j k l m n o p q r s u v w x y z))
(defun get-symbol(taken-symbols)
  (car (remove-if (lambda (s) (member s taken-symbols)) symbol-bank)))

(defun sub(expr old new)
  (switch-expr expr
               (if (eq old expr) new expr)
               (if (eq old arg)
                 expr
                 (let ((fvn (fv new)))
                  (if (member arg fvn)
                      (let ((z (get-symbol (append fvn (fv exp)))))
                        (to-λ z (sub (sub exp arg z) old new)))
                      (to-λ arg (sub exp old new)))))
               (map 'list (lambda (e) (sub e old new)) expr)))
