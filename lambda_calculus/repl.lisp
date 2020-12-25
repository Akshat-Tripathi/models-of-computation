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

(defun expand-constants(expr)
  (switch-expr-restore-brkt expr
                            (let ((val (gethash expr constant-to-λ)))
                              (if val
                                val
                                expr))
                            (to-λ arg (expand-constants exp))
                            (map 'list #'expand-constants expr)))

(defun collapse-constants(expr)
  (let ((val (gethash expr λ-to-constant)))
    (if val
      val
      (switch-expr-restore-brkt expr
                                expr
                                (to-λ arg (collapse-constants exp))
                                (map 'list #'collapse-constants expr)))))

(defun read-expr-from-string(str)
  (read-from-string
   (format nil "(~a)" ;Bracket expr
           (replace-all "  " " " ;Remove all double spaces (also allows . without spaces)
                        (replace-all "." " |.| " str))))) ;Escape dots
