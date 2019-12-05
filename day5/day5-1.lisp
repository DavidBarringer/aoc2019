(defvar outputbuffer)
(defvar l)
(defvar p)

(defun operate (op v x y)
  (cond ((= op 1) (incf p 4)(store (+ x y) (CADDDR v)))
        ((= op 2) (incf p 4)(store (* x y) (CADDDR v)))
        ((= op 3) (incf p 2)(input (CADR v)))
        ((= op 4) (incf p 2)(output x)))
)

(defun input (pos)
  (setq i (read))
  (store i pos)
)

(defun immediatecheck (v x pos)
  (if (null x) (CAR (nthcdr pos v)) (loc (CAR (nthcdr pos v))))
)

(defun tonum (x)
  (parse-integer (format nil "~{~A~}" x))
)

(defun immediatetoop (v)
  (setq instruction (numtolist (CAR v)))
  (loop for i from (list-length instruction) to 5 do(push 0 instruction))
  (setq op (tonum (remove 0 (last instruction 2) :test `=)))
  (setq x (immediatecheck v (= 0 (CAR (nthcdr 2 (reverse instruction)))) 1))
  (setq y (immediatecheck v (= 0 (CAR (nthcdr 3 (reverse instruction)))) 2))
  (operate op v x y)
)

(defun numtolist (n)
  (loop for c across (write-to-string n) collect (digit-char-p c))
)

(defun output (x)
  (push x outputbuffer)
  (intcode)
)

(defun store (x loc)
  (cond ((null loc) l)
        (t (setf (nth loc l) x) (intcode)))
)

(defun loc (x)
  (if (null x) nil (CAR (nthcdr x l)))
)

(defun intcode ()
  (if (> p (list-length l)) l (immediatetoop (nthcdr p l)))
)

(defun run ()
  (setq outputbuffer `())
  (setq p 0)
  (setq l (loop for j in (CAR (cl-csv:read-csv #P"day5/data5.csv")) collect (parse-integer j)))
  (intcode)
  (reverse outputbuffer)
)
