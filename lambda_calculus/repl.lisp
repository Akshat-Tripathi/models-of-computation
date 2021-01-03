(ql:quickload "str")
(ql:quickload "uiop")
(use-package :str)
(load "core.lisp" :external-format :utf-8)
(load "types.lisp" :external-format :utf-8)
(load "engine.lisp" :external-format :utf-8)
;;Register constants
(defparameter constant-to-λ (make-hash-table))
(defparameter λ-to-constant (make-hash-table :test 'equal))

(defun register-constant(constant λ)
 (let ((lam (setf (gethash constant constant-to-λ) λ)))
   (remhash lam λ-to-constant))
 (setf (gethash λ λ-to-constant) constant))

(register-constant 'pair pair)
(register-constant 'fst fst)
(register-constant 'snd snd)

(register-constant 'null nil-λ)
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
  (labels ((expand-constants-helper(expr bound)
                                   (switch-expr-restore-brkt
                                    expr
                                    (let ((val (gethash expr constant-to-λ)))
                                      (if (and (not (member expr bound)) val)
                                        val
                                        expr))
                                    (to-λ arg (expand-constants-helper exp (cons arg bound)))
                                    (map 'list (lambda (expr) (expand-constants-helper expr bound)) expr))))
          (expand-constants-helper expr nil)))

;;For each expression, check if it corresponds to a constant, if so replace it
(defun collapse-constants(expr)
  (switch-expr-restore-brkt expr
                     expr
                     (let ((val (gethash expr λ-to-constant)))
                       (if val
                         val
                         (to-λ arg (collapse-constants exp))))
                     (let ((val (gethash expr λ-to-constant)))
                       (if val
                         val
                         (map 'list #'collapse-constants expr)))))

;;Syntactic sugar for multiple arguments in a λ, also replaces numbers with the appropriate lambdas
(defun expand-λ(expr)
  (labels ((split-dot(expr start)
                     (if (eq '\. (car expr))
                       (values start (cdr expr))
                       (split-dot (cdr expr) (cons (car expr) start))))
           (from-λ(λ) ;Overrides the from-λ used by switch-expr
                  (split-dot (cdr λ) '())))
          (switch-expr-restore-brkt expr
                                    (if (numberp expr)
                                      (encode-nat expr)
                                      expr)
                                    (reduce (lambda (x y) (to-λ y x))
                                            (append (expand-λ exp) arg)) ;arg is the list of args
                                    (map 'list #'expand-λ expr))))

(defun read-expr-from-string(str)
  (expand-constants
   (expand-λ
    (read-from-string
     (format nil "(~a)" ;Bracket expr
      (replace-all "  " " " ;Remove all double spaces (also allows . without spaces)
       (replace-all "λ" " λ " ;Allow λx
        (replace-all "lambda" "λ" ;Allow lambda to be substituted for λ
         (replace-all "." " |.| " str))))))))) ;Escape dots

(defparameter files '())
(defun load-λ(file-name)
  (if (not (member file-name files :test #'string-equal))
    (setf files (cons file-name files)))
  (let ((text (join "" (uiop:read-file-lines file-name))))
    (map nil #'process-cmd (remove-if #'empty? (split ";" text)))))

;;Decides what to do for some input
;;Commands:
;; :l <f> - loads a file
;; :r - reloads all files
;; :q - quits
;; :e <expr> - evaluates an expression without simplification
;; :s <var> <expr> - sets a variable (can override an existing variable)
;; <expr> - evaluates the expression
(defun process-cmd(str)
 (if (empty? str)
     nil
     (cond ((starts-with? ":l" str) (load-λ (string-left-trim ":l " str)))
      ((starts-with? ":r" str) (progn (map 'list #'load-λ files) t))
      ((starts-with? ":q" str) (exit))
      ((starts-with? ":e" str) (print-expr (normal-order (read-expr-from-string (string-left-trim ":e " str)))))
      ((starts-with? ":s" str) (let* ((tokens (split " " (string-left-trim ":s " str)))
                                      (var (intern (car tokens)))
                                      (expr (join " " (cdr tokens))))
                                 (register-constant var (normal-order (read-expr-from-string expr)))))
      (t (print-expr (collapse-constants (normal-order (read-expr-from-string str))))))))

(format t "Hello there - General Kenobi~%")
(force-output t)

(loop
 (format t ">> ")
 (force-output t)
 (process-cmd (read-line)))
