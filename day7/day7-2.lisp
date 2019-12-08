(defvar outputbuffer)
(defvar l)
(defvar p)
(defvar inputs)
(defvar oldlength)
(defvar oginput)

(defun operate (op v x y)
  (cond ((= op 1) (incf p 4) (store (+ x y) (CADDDR v)))
        ((= op 2) (incf p 4) (store (* x y) (CADDDR v)))
        ((= op 3) (incf p 2) (input (CADR v)))
        ((= op 4) (incf p 2) (push x outputbuffer) (intcode))
        ((= op 5) (incf p 3) (jumpt x y))
        ((= op 6) (incf p 3) (jumpf x y))
        ((= op 7) (incf p 4) (store (check `< x y) (CADDDR v)))
        ((= op 8) (incf p 4) (store (check `= x y) (CADDDR v)))
        ((= op 99) (store nil nil))))

(defun all-permutations (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
             append (mapcar (lambda (l) (cons element l))
                            (all-permutations (remove element list)))))))

(defun input (pos)
  (if (null inputs) (store nil nil) (store (pop inputs) pos)))

(defun check (f x y)
  (if (funcall f x y) 1 0))

(defun jumpt (x y)
  (cond ((= 0 x) (intcode))
        (t (setq p y) (intcode)))
)

(defun jumpf (x y)
  (cond ((= 0 x) (setq p y) (intcode))
        (t (intcode))))

(defun immediatecheck (v x pos)
  (if (null x) (CAR (nthcdr pos v)) (loc (CAR (nthcdr pos v)))))

(defun tonum (x)
  (parse-integer (format nil "~{~A~}" x)))

(defun immediatetoop (v)
  (setq instruction (numtolist (CAR v)))
  (loop for i from (list-length instruction) to 5 do(push 0 instruction))
  (setq op (tonum (remove 0 (last instruction 2) :test `=)))
  (setq x (immediatecheck v (= 0 (CADDR (reverse instruction))) 1))
  (setq y (immediatecheck v (= 0 (CADDDR (reverse instruction))) 2))
  (operate op v x y))

(defun numtolist (n)
  (loop for c across (write-to-string n) collect (digit-char-p c)))

(defun store (x loc)
  (cond ((null loc) l)
        (t (setf (nth loc l) x) (intcode))))

(defun loc (x)
  (if (null x) nil (CAR (nthcdr x l))))

(defun intcode ()
  (if (> p (list-length l)) l (immediatetoop (nthcdr p l))))

(defun nextinput (c i)
  (setq oginput c)
  (setq outputbuffer `())
  (setq oldlength 0)
  (newinput c i))

(defun newinput (c i)
  (setq inputs (cons (pop c) (reverse i)))
  (setq l (loop for j in (CAR (cl-csv:read-csv #P"day7/data7.csv")) collect (parse-integer j)))
  (setq p 0)
  (setq outputbuffer `())
  (intcode)
  (cond ((AND (null c) (= (list-length outputbuffer) oldlength)) (CAR outputbuffer))
        ((null c) (setq oldlength (list-length outputbuffer))(newinput oginput (append outputbuffer `(0))))
        (t (newinput c outputbuffer))))

(defun run2 ()
  (apply `max (loop for c in (all-permutations `(5 6 7 8 9)) collect (nextinput c `(0)))))
