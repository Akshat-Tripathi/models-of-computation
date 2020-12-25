(load "core.lisp" :external-format :utf-8)

(defun parse(expr)
  (terpri)
  (labels ((next(indent)
                (format nil "--~a" indent))
           (parse-helper(expr indent)
                        (switch-expr-dbrkt expr
                                           (format t "~avar ~a~%" indent expr)
                                           (progn
                                             (format t "~aabs ~a~%" indent expr)
                                             (parse-helper (car exp) (next indent)))
                                           (progn
                                             (format t "~aapp ~a~%" indent expr)
                                             (parse-helper (car expr) (next indent))
                                             (parse-helper (cdr expr) (next indent))))))
          (parse-helper expr "")))



(defparameter abstraction '(λ x \. λ y \. x y z))
(defvar app '(x y z))
(defvar var1 '(x))
(defvar var2 'x)

(print "parse tests")
(terpri)
(parse abstraction)
(parse app)
(parse var1)
(parse var2)

(print "free variable tests")
(print (fv abstraction))
(print (fv app))
(print (fv var1))
(print (fv var2))

(print "sub tests")
(print (print-expr (sub abstraction 'z 'i)))
(print (print-expr (sub abstraction 'y 'i)))
(print (print-expr (sub abstraction 'z 'y)))
(print (print-expr (sub app 'z 'i)))
(print (print-expr (sub var1 'x 'i)))
(print (print-expr (sub var2 'x 'i)))

;;Engine tests
; (load "engine.lisp")
; (defparameter redex
;   '((λ x \. λ y \. x y z) a b))
;
; (print (is-redex redex))
; (print (is-redex app))
