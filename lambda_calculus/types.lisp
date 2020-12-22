(load "core.lisp" :external-format :utf-8)

(defun args-to-λ(args expr)
  (if (null args)
    expr
    (to-λ (car args) (args-to-λ (cdr args) expr))))


;;Pairs
;; (a, b) -> λp. a b

(defconstant pair
  (args-to-λ '(a b p) '(p a b)))

(defconstant fst
  (to-λ 'p (cons 'p (list (args-to-λ '(a b) '(a))))))

(defconstant snd
  (to-λ 'p (cons 'p (list (args-to-λ '(a b) '(b))))))

;;Lists
(defconstant none
  (args-to-λ '(c n) '(n)))

(defconstant kons
  (args-to-λ '(e l c n) '(c e l)))
;;Integers
