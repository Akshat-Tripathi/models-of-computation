(load "core.lisp" :external-format :utf-8)

;;Pairs
;; (a, b) -> λp. a b

(defconstant pair
  (to-λ 'a (to-λ 'b (to-λ 'p '(p a b)))))

(defconstant fst
  (to-λ 'p (cons 'p (list (to-λ 'a (to-λ 'b '(a)))))))

(defconstant snd
  (to-λ 'p (cons 'p (list (to-λ 'a (to-λ 'b '(b)))))))

;;Integers
