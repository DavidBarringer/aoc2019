(defvar outputbuffer)
(defvar l)
(defvar p)

(defun operate (op v x y)
  (cond ((= op 1) (incf p 4) (store (+ x y) (CADDDR v)))
        ((= op 2) (incf p 4) (store (* x y) (CADDDR v)))
        ((= op 3) (incf p 2) (setq i (read)) (store i (CADR v)))
        ((= op 4) (incf p 2) (push x outputbuffer) (intcode))
        ((= op 5) (incf p 3) (jumpt x y))
        ((= op 6) (incf p 3) (jumpf x y))
        ((= op 7) (incf p 4) (store (check `< x y) (CADDDR v)))
        ((= op 8) (incf p 4) (store (check `= x y) (CADDDR v)))
        ((= op 99) (store nil nil))))

(defun check (f x y)
  (if (funcall f x y) 1 0))

(defun jumpt (x y)
  (cond ((= 0 x) (intcode))
        (t (setq p y) (intcode))))

(defun jumpf (x y)
  (cond ((= 0 x) (setq p y) (intcode))
        (t (intcode))))

(defun immediatecheck (v x pos)
  (if (null x) (nth pos v) (loc (nth pos v))))

(defun immediatetoop (v)
  (setq instruction (numtolist (CAR v)))
  (loop for i from (list-length instruction) to 5 do(push 0 instruction))
  (setq op (tonum (last instruction 2)))
  (setq x (immediatecheck v (= 0 (CADDDR instruction)) 1))
  (setq y (immediatecheck v (= 0 (CADDR instruction)) 2))
  (operate op v x y))

(defun store (x loc)
  (cond ((null loc) l)
        (t (setf (nth loc l) x) (intcode))))

(defun loc (x)
  (if (null x) nil (nth x l)))

(defun intcode ()
  (if (> p (list-length l)) l (immediatetoop (nthcdr p l))))

(defun run2 ()
  (setq outputbuffer `())
  (setq p 0)
  (setq l (loop for j in (CAR (cl-csv:read-csv #P"day5/data5.csv")) collect (parse-integer j)))
  (intcode)
  (reverse outputbuffer))
