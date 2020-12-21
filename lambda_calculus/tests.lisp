(load "core.lisp")

(defun parse(expr)
  (print expr)
  (switch-expr expr
               (progn
                (prin1 "var")
                (terpri))
               (progn
                 (prin1 "abs")
                 (parse exp))
               (progn
                 (prin1 "app")
                 (parse (car expr))
                 (parse (cdr expr)))))

(defparameter abstraction '(lam x \. lam y \. x y z))
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
;   '((lam x \. lam y \. x y z) a b))
;
; (print (is-redex redex))
; (print (is-redex app))
