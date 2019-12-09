(defvar outputbuffer)
(defvar l)
(defvar p)
(defvar inputs)

(defun operate (op v x y)
  (cond ((= op 1) (incf p 4) (store (+ x y) (CADDDR v)))
        ((= op 2) (incf p 4) (store (* x y) (CADDDR v)))
        ((= op 3) (incf p 2) (store (pop inputs) (CADR v)))
        ((= op 4) (incf p 2) (push x outputbuffer) (intcode))
        ((= op 5) (incf p 3) (jumpt x y))
        ((= op 6) (incf p 3) (jumpf x y))
        ((= op 7) (incf p 4) (store (check `< x y) (CADDDR v)))
        ((= op 8) (incf p 4) (store (check `= x y) (CADDDR v)))
        ((= op 99) (store nil nil))))

(defun all-permutations (list)
  (cond ((null (cdr list)) (list list))
        (t (loop for element in list
             append (mapcar (lambda (l) (cons element l))
                            (all-permutations (remove element list)))))))

(defun check (f x y)
  (if (funcall f x y) 1 0))

(defun jumpt (x y)
  (cond ((= 0 x) (intcode))
        (t (setq p y) (intcode))))

(defun jumpf (x y)
  (cond ((= 0 x) (setq p y) (intcode))
        (t (intcode))))

(defun immediatecheck (v x pos)
  (if (null x) (CAR (nthcdr pos v)) (loc (CAR (nthcdr pos v)))))

(defun immediatetoop (v)
  (setq instruction (numtolist (CAR v)))
  (loop for i from (list-length instruction) to 5 do (push 0 instruction))
  (setq op (tonum (remove 0 (last instruction 2) :test `=)))
  (setq x (immediatecheck v (= 0 (CADDR (reverse instruction))) 1))
  (setq y (immediatecheck v (= 0 (CADDDR (reverse instruction))) 2))
  (operate op v x y))

(defun store (x loc)
  (cond ((null loc) l)
        (t (setf (nth loc l) x) (intcode))))

(defun loc (x)
  (if (null x) nil (CAR (nthcdr x l))))

(defun intcode ()
  (if (> p (list-length l)) l (immediatetoop (nthcdr p l))))

(defun newinput (c i)
  (setq inputs (cons (CAR c) (cons i (CDR c)))) ; puts the input to machine after the setting
  (setq outputbuffer `())
  (setq l (loop for j in (CAR (cl-csv:read-csv #P"day7/data7.csv")) collect (parse-integer j)))
  (setq p 0)
  (intcode)
  ; passes output to next machine until all inputs are exhausted
  (if (null inputs) (CAR outputbuffer) (newinput inputs (pop outputbuffer))))

(defun run ()
  (apply `max (loop for c in (all-permutations `(0 1 2 3 4)) collect (newinput c 0))))
