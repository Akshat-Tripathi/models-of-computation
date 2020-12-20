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
  (switch-expr expr
               expr
               (set-difference (fv exp) args)
               (remove-duplicates (map 'list #'fv expr))))

(defun member-fv(item expr)
  (let ((free (fv expr)))
    (match free
           ((cons _ _) (member item free))
           (_ (eq item free)))))

;;TODO alpha equivalence

;;Substitution
(defvar symbol-bank '(nil a b c d e f g h i j k l m n o p)) ;TODO replace this with an infinite list of integers
(defun get-symbol()
  (setf symbol-bank (cdr symbol-bank))
  (car symbol-bank))

(defun sub(expr old new)
  (switch-expr expr
               (if (eq old expr) new expr)
               (if (member old args)
                 expr
                 (let* ((old* (remove-if-not (lambda (x) (member-fv x (fv new))) args))
                        (new* (map 'list (lambda (x) (list x (get-symbol))) old*)))
                   (make-λ (reduce (lambda (a b) (sub a (car b) (cadr b))) (cons args new*))
                       (sub (reduce (lambda (a b) (sub a (car b) (cadr b))) (cons exp new*))
                            old
                            new))))
               (map 'list (lambda (e) (sub e old new)) expr)))
