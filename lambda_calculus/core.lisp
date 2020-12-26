(ql:quickload "trivia")
(use-package :trivia)

;; 3 types of expression
;; 1. Variables i.e. x
;; 2. Abstractions i.e. λ x . x
;; 3. Applications i.e. M M

(defun to-λ(arg expr)
  (cons 'λ (cons arg (cons '\. expr))))

(defun from-λ(λ)
  (values (cadr λ) (list (cdddr λ))))


(defun args-to-λ(args expr)
  (if (null args)
    expr
    (to-λ (car args) (args-to-λ (cdr args) expr))))


(defun to-app(&rest exprs)
  exprs)

(defmacro switch-expr(expr var abs app)
  `(match ,expr
         ((cons 'λ _) (multiple-value-bind (arg exp) (from-λ expr) ,abs))
         ((cons _ _) ,app)
         (_ ,var)))

(defmacro switch-expr-restore-brkt(expr var abs app)
  (defun debrkt-count(expr n)
    (if (and (listp expr) (null (cdr expr)))
      (debrkt-count (car expr) (incf n))
      (values expr n)))
  (defun restore-brkt(expr n)
    (if (zerop n)
      expr
      (restore-brkt (list expr) (decf n))))
  `(multiple-value-bind (expr n) (debrkt-count ,expr 0)
                        (restore-brkt (switch-expr expr ,var ,abs ,app) n)))

;;Use this to pattern match expressions and remove brackets
(defmacro switch-expr-dbrkt(expr var abs app)
 (defun debrkt(expr)
   (if (and (listp expr) (null (cdr expr)))
     (debrkt (car expr))
     expr))
 `(let ((expr (debrkt ,expr)))
   (switch-expr expr ,var ,abs ,app)))

(defun print-expr (expr)
  (labels
   ((print-expr-helper(expr in-abs)
                      (format nil "~a" (switch-expr-restore-brkt expr
                                                   (format nil "~a" (string-downcase (symbol-name expr)))
                                                   (format nil "λ~a.~a" (string-downcase (symbol-name arg)) (print-expr-helper exp t))
                                                   (format nil (if in-abs
                                                                 "~{~a~^ ~}"
                                                                 "(~{~a~^ ~})")
                                                           (map 'list (lambda (expr) (print-expr-helper expr nil)) expr))))))
   (format t "~a~%" (print-expr-helper expr nil))
   (finish-output t)))

;;Finds all the free variables in an expression
(defun fv(expr)
  (let ((free (switch-expr-dbrkt expr
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
  (switch-expr-dbrkt expr
               (if (eq old expr) new expr)
               (if (eq old arg)
                 expr
                 (let ((fvn (fv new)))
                  (if (member arg fvn)
                      (let ((z (get-symbol (append fvn (fv exp)))))
                        (to-λ z (sub (sub exp arg z) old new)))
                      (to-λ arg (sub exp old new)))))
               (map 'list (lambda (e) (sub e old new)) expr)))
