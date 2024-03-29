(defvar outputbuffer)
(defvar l)
(defvar p)
(defvar rbase)

(defun operate (op v x y z)
  (cond ((= op 1) (incf p 4) (store (+ (modecheck v x 1 nil) (modecheck v y 2 nil)) (modecheck v z 3 t)))
        ((= op 2) (incf p 4) (store (* (modecheck v x 1 nil) (modecheck v y 2 nil)) (modecheck v z 3 t)))
        ((= op 3) (incf p 2) (setq i (read)) (store i (modecheck v x 1 t)))
        ((= op 4) (incf p 2) (push (modecheck v x 1 nil) outputbuffer) (intcode))
        ((= op 5) (incf p 3) (jumpt (modecheck v x 1 nil) (modecheck v y 2 nil)))
        ((= op 6) (incf p 3) (jumpf (modecheck v x 1 nil) (modecheck v y 2 nil)))
        ((= op 7) (incf p 4) (store (check `< (modecheck v x 1 nil) (modecheck v y 2 nil)) (modecheck v z 3 t)))
        ((= op 8) (incf p 4) (store (check `= (modecheck v x 1 nil) (modecheck v y 2 nil)) (modecheck v z 3 t)))
        ((= op 9) (incf p 2) (incf rbase (modecheck v x 1 nil)) (intcode))
        ((= op 99) (store nil nil))))

(defun check (f x y)
  (if (funcall f x y) 1 0))

(defun jumpt (x y)
  (cond ((= 0 x) (intcode))
        (t (setq p y) (intcode))))

(defun jumpf (x y)
  (cond ((= 0 x) (setq p y) (intcode))
        (t (intcode))))

; increases the memory given to the machine, returning the result if the memory exists
; if the value is going to be stored at the position, return the position, not the value at position
(defun memcheck (pos s)
  (cond ((> (+ pos 1) (list-length l)) (nconc l (make-list (- (+ pos 1) (list-length l)) :initial-element 0))
                                       (memcheck pos s))
        ((null s) (loc pos))
        (t pos)))

(defun modecheck (v x pos s)
  (cond ((= x 0) (memcheck (nth pos v) s))
        ((= x 1) (nth pos v))
        ((= x 2) (memcheck (+ rbase (nth pos v)) s))))

(defun immediatetoop (v)
  (setq instruction (numtolist (CAR v)))
  (loop for i from (list-length instruction) to 5 do (push 0 instruction))
  (setq op (tonum (last instruction 2)))
  (setq x (CADDDR instruction))
  (setq y (CADDR instruction))
  (setq z (CADR instruction))
  (operate op v x y z))

; same as old store, but with memory increasing
(defun store (x pos)
  (cond ((null pos) l)
        ((> (+ pos 1) (list-length l)) (nconc l (make-list (- (+ pos 1) (list-length l)) :initial-element 0))
                                       (store x pos))
        (t (setf (nth pos l) x) (intcode))))

(defun loc (x)
  (if (null x) nil (nth x l)))

(defun intcode ()
  (if (> p (list-length l)) l (immediatetoop (nthcdr p l))))

(defun run ()
  (setq outputbuffer `())
  (setq p 0)
  (setq rbase 0)
  (setq l (loop for j in (CAR (cl-csv:read-csv #P"day9/data9.csv")) collect (parse-integer j)))
  (intcode)
  (reverse outputbuffer))
