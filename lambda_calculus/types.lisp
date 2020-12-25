(load "core.lisp" :external-format :utf-8)

;;Pairs
;; (a, b) -> λp. a b

(defconstant pair
  (args-to-λ '(a b p) '(p a b)))

(defconstant fst
  (to-λ 'p (cons 'p (list (args-to-λ '(a b) '(a))))))

(defconstant snd
  (to-λ 'p (cons 'p (list (args-to-λ '(a b) '(b))))))

;;Lists
(defconstant nil-λ
  (args-to-λ '(c n) '(n)))

(defconstant cons-λ
  (args-to-λ '(e l c n) '(c e l)))

;;Integers
(defconstant zero
  (args-to-λ '(s z) 'z))

(defconstant succ
  (args-to-λ '(n f z) (cons 'f (list (to-app 'n 'f 'z)))))

(defconstant add-λ
  (args-to-λ '(a b f z) (list 'a 'f (list 'b 'f 'z))))

(defconstant mult
  (args-to-λ '(a b f) (list 'a (list 'b 'f))))

(defun encode-nat(n)
  (labels ((f(n nat)
             (if (<= n 0)
               nat
               (f (- n 1) (to-app 'f nat)))))
          (args-to-λ '(f z) (f n 'z))))

;;Booleans
(defconstant true
  (args-to-λ '(x y) 'x))

(defconstant false
  (args-to-λ '(x y) 'y))

(defconstant if-λ
  (args-to-λ '(b x y) '(b x y)))

(defconstant and-λ
  (args-to-λ '(b1 b2) (list 'b1 'b2 false)))

(defconstant or-λ
  (args-to-λ '(b1 b2) (list 'b1 true 'b2)))

(defconstant not-λ
  (to-λ 'b (list 'b false true)))
