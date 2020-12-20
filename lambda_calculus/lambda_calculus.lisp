(ql:quickload "trivia")
(use-package :trivia)

;; 3 types of expression
;; 1. Variables i.e. x
;; 2. Abstractions i.e. λ x . x
;; 3. Applications i.e. M M

(defun make-λ(args expr)
  (append (cons 'λ args) (cons '\. expr)))

(defun split-dot-helper(l start)
  (match l
         ((cons '\. rest) (values start rest))
         ((cons x y) (split-dot-helper y (nconc start (list x))))))

(defun split-dot(l)
  (split-dot-helper l (list)))

;;Use this to pattern match expressions
(defmacro switch-expr(expr var abs app)
  `(match ,expr
           ((cons expr nil) ,var)
           ((cons 'λ _) (multiple-value-bind (args exp) (split-dot (cdr ,expr)) ,abs))
           ((cons _ _) ,app)
           (_ ,var)))

;;Finds all the free variables in an expression
(defun fv(expr)
  (let ((free (switch-expr expr
               expr
               (set-difference (fv exp) args)
               (remove-duplicates (map 'list #'fv expr)))))
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
               (if (member old args)
                 expr
                 (let* ((fvn (fv new))
                        (old* (remove-if-not (lambda (x) (member x fvn)) args))
                        (new* (map 'list (lambda (x) (list x (get-symbol (append fvn (fv exp))))) old*)))
                   (make-λ (reduce (lambda (a b) (sub a (car b) (cadr b))) (cons args new*))
                       (sub (reduce (lambda (a b) (sub a (car b) (cadr b))) (cons exp new*))
                            old
                            new))))
               (map 'list (lambda (e) (sub e old new)) expr)))
