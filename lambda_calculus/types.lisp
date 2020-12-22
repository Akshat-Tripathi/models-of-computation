(load "core.lisp" :external-format :utf-8)

;;Pairs
;; (a, b) -> λp. a b

(defun pair(a b)
  (to-λ 'p (list 'p a b)))

(defun fst()
  (to-λ 'p (cons 'p (list (to-λ 'a (to-λ 'b '(a)))))))

(defun snd()
  (to-λ 'p (cons 'p (list (to-λ 'a (to-λ 'b '(b)))))))

(defparameter test-pair (pair 'a 'b))
(defparameter test-fst (list (snd) test-pair))
