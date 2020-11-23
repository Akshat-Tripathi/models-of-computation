(defun pair-encode(x y)
    (* (ash 1 x) (+ (* 2 y) 1)))

(defun pair-decode(z)
    (do ((x 0 (+ x 1))
         (y z (/ y 2)))
        ((/= (mod y 2) 0) (values x (/ (- y 1) 2)))))

(defun list-encode(l)
    (match l 
        (cons x xs)))