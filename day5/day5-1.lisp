(defvar outputbuffer)
; machine and pointer are defined as global variables
(defvar l)
(defvar p)

(defun operate (op v x y)
  (cond ((= op 1) (incf p 4)(store (+ x y) (CADDDR v)))
        ((= op 2) (incf p 4)(store (* x y) (CADDDR v)))
        ((= op 3) (incf p 2) (setq i (read)) (store i (CADR v))) ; lets user type input
        ((= op 4) (incf p 2) (push x outputbuffer) (intcode))
        ((= op 99) (store nil nil))))

(defun immediatecheck (v x pos)
  (if (null x) (CAR (nthcdr pos v)) (loc (CAR (nthcdr pos v)))))

(defun immediatetoop (v)
  (setq instruction (numtolist (CAR v)))
  (loop for i from (list-length instruction) to 5 do (push 0 instruction)) ; pads instruction with 0s
  (setq op (tonum (last instruction 2))) ; gets opcode from last 2 values in instruction
  (setq x (immediatecheck v (= 0 (CADDDR instruction)) 1))
  (setq y (immediatecheck v (= 0 (CADDR instruction)) 2))
  (operate op v x y))

; better store function, shouldn't need to remake the whole list
(defun store (x loc)
  (cond ((null loc) l)
        (t (setf (nth loc l) x) (intcode))))

(defun loc (x)
  (if (null x) nil (nth x l)))

(defun intcode ()
  (if (> p (list-length l)) l (immediatetoop (nthcdr p l))))

(defun run ()
  (setq outputbuffer `())
  (setq p 0)
  (setq l (loop for j in (CAR (cl-csv:read-csv #P"day5/data5.csv")) collect (parse-integer j)))
  (intcode)
  (reverse outputbuffer))
