(defvar outputbuffer)
(defvar l)

(defun operate (p op v x y)
  (cond ((= op 1) (store (+ p 4) (+ x y) (CADDDR v)))
        ((= op 2) (store (+ p 4) (* x y) (CADDDR v)))
        ((= op 3) (input (+ p 2) (CADR v)))
        ((= op 4) (output (+ p 2) x))
        ((= op 5) (jumpt (+ p 3) x y))
        ((= op 6) (jumpf (+ p 3) x y))
        ((= op 7) (store (+ p 4) (check `< x y) (CADDDR v)))
        ((= op 8) (store (+ p 4) (check `= x y) (CADDDR v)))
        ((= op 99) (store p nil nil))
  )
)

(defun input (p pos)
  (setq i (read))
  (store p i pos)
)

(defun check (f x y)
  (if (funcall f x y) 1 0)
)

(defun jumpt (p x y)
  (if (= 0 x) (intcode p) (intcode y))
)

(defun jumpf (p x y)
  (if (= 0 x) (intcode y) (intcode p))
)

(defun immediatecheck (v x pos)
  (if (null x) (CAR (nthcdr pos v)) (loc (CAR (nthcdr pos v))))
)

(defun tonum (x)
  (parse-integer (format nil "~{~A~}" x))
)

(defun immediatetoop (p v)
  (setq instruction (numtolist (CAR v)))
  (loop for i from (list-length instruction) to 5 do(push 0 instruction))
  (setq op (tonum (remove 0 (last instruction 2) :test `=)))
  (setq x (immediatecheck v (= 0 (CAR (nthcdr 2 (reverse instruction)))) 1))
  (setq y (immediatecheck v (= 0 (CAR (nthcdr 3 (reverse instruction)))) 2))
  (operate p op v x y)
)

(defun numtolist (n)
  (loop for c across (write-to-string n) collect (digit-char-p c))
)

(defun output (p x)
  (push x outputbuffer)
  (intcode p)
)

(defun store (p x loc)
  (if (null loc) l (store2 p x loc))
)

(defun store2 (p x loc)
  (setf (nth loc l) x)
  (intcode p)
)

(defun loc (x)
  (if (null x) nil
    (CAR (nthcdr x l))
  )
)

(defun intcode (p)
  (if (> p (list-length l)) l (immediatetoop p (nthcdr p l)))
)

(defun run2 ()
  (setq outputbuffer `())
  (setq l (loop for j in (CAR (cl-csv:read-csv #P"day5/data5.csv")) collect (parse-integer j)))
  (intcode 0)
  (reverse outputbuffer)
)
