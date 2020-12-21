(ql:quickload "trivia")
(use-package :trivia)

;; 3 types of expression
;; 1. Variables i.e. x
;; 2. Abstractions i.e. lam x . x
;; 3. Applications i.e. M M

(defun to-lam(arg expr)
  (cons 'lam (cons arg (cons '\. expr))))

(defun from-lam(lam)
  (values (cadr lam) (cdddr lam)))

;;Use this to pattern match expressions
(defmacro switch-expr(expr var abs app)
 (defun debracket(expr)
   (if (and (listp expr) (null (cdr expr)))
     (debracket (car expr))
     expr))
 `(let ((expr (debracket ,expr)))
   (match expr
          ((cons 'lam _) (multiple-value-bind (arg exp) (from-lam expr) ,abs))
          ((cons _ _) ,app)
          (_ ,var))))

(defun print-expr-helper(expr in-abs)
 (if (and (listp expr) (null (cdr expr)))
  (progn (print "debracket") (format nil "(~a)" (print-expr-helper (car expr) in-abs)))
  (switch-expr expr
               (format nil "~a" (string-downcase (symbol-name expr)))
               (format nil (if in-abs "(λ~a.~a)" "λ~a.~a") (string-downcase (symbol-name arg)) (print-expr-helper exp t))
               (format nil (if in-abs "~{~a~^ ~}" "(~{~a~^ ~})") (map 'list (lambda (expr) (print-expr-helper expr in-abs)) expr)))))

(defun print-expr (expr)
  (print-expr-helper expr nil))

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
                        (to-lam z (sub (sub exp arg z) old new)))
                      (to-lam arg (sub exp old new)))))
               (map 'list (lambda (e) (sub e old new)) expr)))
