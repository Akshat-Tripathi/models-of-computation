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
