(defun pair-encode(x y)
    (* (ash 1 x) (+ (* 2 y) 1)))

(defun pair-decode(z)
    (do ((x 0 (+ x 1))
         (y z (/ y 2)))
        ((/= (mod y 2) 0) (values x (/ (- y 1) 2)))))

(defun list-encode(l)
    (if (null l) 
        0 
        (pair-encode (car l) (list-encode (cdr l)))))

(defun list-decode(n)
    (if (= n 0)
        nil
        (multiple-value-bind (x xs) (pair-decode n) 
            (cons x (list-decode xs)))))

;; The symbol (+ 1 2) represents the instruction r1+ -> l2
;; The symbol (- 1 2 3) represents the instruction r1- -> l2 l3
;; The symbol halt represents the instruction halt

(defmacro unpack(tup vals expr)
    `(apply #'(lambda ,vals ,expr) ,tup))

(defun command-encode(cmd)
    (if (eq cmd 'halt)
        0
        (if (eq (car cmd) '+)
            (unpack (cdr cmd) (reg n) (pair-encode (* 2 reg) n))
            (unpack (cdr cmd) (reg j k) (pair-encode (+ (* 2 reg) 1) (- (pair-encode j k) 1))))))
    
(defun command-decode(code)
    (if (= code 0)
        'halt
        (multiple-value-bind (x y) (pair-decode code)
            (if (= (mod x 2) 0)
                `('+ ,(/ x 2) ,y)
                (multiple-value-bind (j k) (pair-decode (+ y 1)) 
                    `('- ,(/ (- x 1) 2) ,j ,k))))))

(defun program-encode(program)
    (map 'list #' command-encode program))

(defun program-decode(code)
    (map 'list #' command-decode code))