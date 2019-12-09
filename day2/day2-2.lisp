(defun operate (op x y)
  (if (= op 1) (+ x y)
    (if (= op 2) (* x y)
      (if (= op 99) nil))))

(defun store (x loc l)
  (if (null loc) l
    (loop for i from 0 for j in l collect (if (= i loc) x j))))

(defun loc (x l)
  (if (null x) nil (nth x l)))

(defun run (p l)
  (setf v (nthcdr p l))
  (if (> p (list-length l)) l
    (run (+ 4 p) (store
      (operate (CAR v) (loc (CADR v) l) (loc (CADDR v) l))
      (CADDDR v)
      l))))

(defun makelist ()
  (loop for i from 0 for j in (CAR (cl-csv:read-csv #P"day2/data2.csv")) collect (parse-integer j)))

(defun newlist (l noun verb)
  (store verb 2 (store noun 1 l)))

(defun run2 ()
  (setf l (makelist))
  ; nested loops for input
  (loop for i from 0 to 99 do (loop for j from 0 to 99
      if (= 19690720 (CAR (run 0 (newlist l i j)))) do (print (+ j (* 100 i))))))
