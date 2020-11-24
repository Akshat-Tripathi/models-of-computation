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
;; The symbol hlt represents the instruction hlt

(defmacro unpack(tup args expr)
    `(apply #'(lambda ,args ,expr) ,tup))

(defmacro switch-cmd(cmd hlt inc dec)
    `(unpack ,cmd (op &optional src next1 next2)
        (case op
            (hlt (,hlt))
            (+ (,inc src next1))
            (- (,dec src next1 next2)))))

(defun command-encode(cmd)
    (switch-cmd cmd (lambda () 0)
                    (lambda (reg n) (pair-encode (* 2 reg) n))
                    (lambda (reg j k) (pair-encode (+ (* 2 reg) 1) (- (pair-encode j k) 1)))))
    
(defun command-decode(code)
    (if (= code 0)
        '(hlt)
        (multiple-value-bind (x y) (pair-decode code)
            (if (= (mod x 2) 0)
                `(+ ,(/ x 2) ,y)
                (multiple-value-bind (j k) (pair-decode (+ y 1)) 
                    `(- ,(/ (- x 1) 2) ,j ,k))))))

(defun program-encode(program)
    (list-encode (map 'list #' command-encode program)))

(defun program-decode(code)
    (map 'list #' command-decode (list-decode code)))

;; Register management functions
;; Registers will be an infinite list, lazily loaded

(defvar regs nil)
(defvar n-regs 0)

(defun reg(n)
    (if (>= n n-regs)
        (progn 
            (setq n-regs (+ n-regs 1))
            (setq regs (append regs '(0)))
            (reg n))
        (nth n regs)))

(defmacro change(reg n to x)
    `(let ((reg (reg ,n)))
        (setf (nth ,n regs) ,x)))

(defun init-regs(l)
    (init-regs-helper l 0))

(defun init-regs-helper(l i)
    (if (null l)
        nil
        (progn (change reg i to (car l)) (init-regs-helper (cdr l) (+ i 1)))))

(defun inc(n)
    (change reg n to (+ (reg n) 1)))

(defun dec(n)
    (change reg n to (- (reg n) 1)))

;; Program execution

(defun program-execute(cmds)
    (program-execute-helper 0 cmds))

(defun program-execute-helper(cmd-index cmds)
    (let ((cmd (nth cmd-index cmds)))
        (if (eq cmd nil)
            (reg 0)
            (switch-cmd cmd (lambda () 0)
                            (lambda (reg next) (inc reg) (program-execute-helper next cmds))
                            (lambda (r next1 next2) (if (= (reg r) 0)
                                                            (program-execute-helper next2 cmds)
                                                            (progn (dec r) (program-execute-helper next1 cmds))))))))

(defvar prog (program-encode '(
    (+ 0 1)
    (+ 0 2)
    (hlt)
)))

(defvar add (program-encode '(
    (- 1 1 2)
    (+ 0 0)
    (- 2 3 4)
    (+ 0 2)
    (hlt)
)))


(program-execute (program-decode add))
(print regs)