(ql:quickload "str")
(use-package :str)
(load "core.lisp"  :external-format :utf-8)
(load "types.lisp"  :external-format :utf-8)

;;Register constants
(defparameter constant-to-λ (make-hash-table))
(defparameter λ-to-constant (make-hash-table))

(defun register-constant(constant λ)
 (setf (gethash λ λ-to-constant) constant)
 (setf (gethash constant constant-to-λ) λ))

(register-constant 'pair pair)
(register-constant 'fst fst)
(register-constant 'snd snd)

(register-constant 'nil nil-λ)
(register-constant 'cons cons-λ)

(register-constant 'succ succ)
(register-constant 'add add-λ)
(register-constant 'mult mult)

(register-constant 'true true)
(register-constant 'false false)
(register-constant 'if if-λ)
(register-constant 'and and-λ)
(register-constant 'or or-λ)
(register-constant 'not not-λ)

;;For each variable check if it's a constant, if so replace it with the appropriate expression
(defun expand-constants(expr)
  (switch-expr-restore-brkt expr
                            (let ((val (gethash expr constant-to-λ)))
                              (if val
                                val
                                expr))
                            (to-λ arg (expand-constants exp))
                            (map 'list #'expand-constants expr)))

;;For each expression, check if it corresponds to a constant, if so replace it
(defun collapse-constants(expr)
  (let ((val (gethash expr λ-to-constant)))
    (if val
      val
      (switch-expr-restore-brkt expr
                                expr
                                (to-λ arg (collapse-constants exp))
                                (map 'list #'collapse-constants expr)))))

;;Syntactic sugar for multiple arguments in a λ
(defun expand-λ(expr)
  (labels ((split-dot(expr start)
                     (if (eq '\. (car expr))
                       (values start (cdr expr))
                       (split-dot (cdr expr) (cons (car expr) start))))
           (from-λ(λ)
                  (split-dot (cdr λ) '())))
          (switch-expr-restore-brkt expr
                                    expr
                                    (reduce (lambda (x y) (to-λ y x))
                                            (append (expand-λ exp) arg))
                                    (map 'list #'expand-λ expr))))


(defun read-expr-from-string(str)
  (expand-λ
   (read-from-string
    (format nil "(~a)" ;Bracket expr
            (replace-all "  " " " ;Remove all double spaces (also allows . without spaces)
              (replace-all "λ" " λ " ;Allow λx
                (replace-all "lambda" "λ" ;Allow lambda to be substituted for λ
                 (replace-all "." " |.| " str)))))))) ;Escape dots
