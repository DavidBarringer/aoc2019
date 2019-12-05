(defvar outputbuffer)

(defun operate (p op l v x y)
  (cond ((= op 1) (store (+ p 4) (+ (immediatecheck l v x 1) (immediatecheck l v y 2)) (CADDDR v) l))
        ((= op 2) (store (+ p 4) (* (immediatecheck l v x 1) (immediatecheck l v y 2)) (CADDDR v) l))
        ((= op 3) (input (+ p 2) (CADR v) l))
        ((= op 4) (output (+ p 2) (immediatecheck l v x 1) l))
        ((= op 99) (store p nil nil l))
  )
)

(defun input (p pos l)
  (setq i (read))
  (store p i pos l)
)

(defun immediatecheck (l v x pos)
  (if (null x) (CAR (nthcdr pos v)) (loc (CAR (nthcdr pos v)) l))
)

(defun tonum (l)
  (parse-integer (format nil "~{~A~}" l))
)

(defun immediatetoop (p l v)
  (setq instruction (numtolist (CAR v)))
  (loop for i from (list-length instruction) to 5 do(push 0 instruction))
  (setq op (tonum (remove 0 (last instruction 2) :test `=)))
  (setq x (= 0 (CAR (nthcdr 2 (reverse instruction)))))
  (setq y (= 0 (CAR (nthcdr 3 (reverse instruction)))))
  (operate p op l v x y)
)

(defun numtolist (n)
  (loop for c across (write-to-string n) collect (digit-char-p c))
)

(defun output (p x l)
  (push x outputbuffer)
  (intcode p l)
)

(defun store (p x loc l)
  (if (null loc) l
    (intcode p (loop for i from 0 for j in l collect (if (= i loc) x j)))
  )
)

(defun loc (x l)
  (if (null x) nil
    (CAR (nthcdr x l))
  )
)

(defun intcode (p l)
  (if (> p (list-length l)) l
    (immediatetoop p l (nthcdr p l))
  )
)

(defun makelist ()
  (loop for i from 0 for j in (CAR (cl-csv:read-csv #P"day5/data5.csv")) collect (parse-integer j))
)

(defun run ()
  (setq outputbuffer `())
  (intcode 0 (makelist))
  (reverse outputbuffer)
)
