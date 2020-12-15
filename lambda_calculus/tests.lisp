(load "lambda_calculus.lisp")

(defun parse(expr)
  (switch-expr expr
               (progn
                (prin1 "var")
                (terpri))
               (progn
                 (prin1 "abs")
                 (parse exp))
               (progn
                 (prin1 "app")
                 (parse (cdr expr)))))

(defvar abstraction '(λ x y \. x y z))
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
(print (sub abstraction 'z 'i))
(print (sub abstraction 'y 'i))
(print (sub abstraction 'z 'y))
(print (sub app 'z 'i))
(print (sub var1 'x 'i))
(print (sub var2 'x 'i))
