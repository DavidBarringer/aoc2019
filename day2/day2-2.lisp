(defun operate (op x y)
  (if (= op 1)
    (+ x y) (if (= op 2)
      (* x y) (if (= op 99)
        nil
      )
    )
  )
)

(defun store (x loc l)
  (if (null loc) l
    (loop for i from 0 for j in l collect (if (= i loc) x j))
  )
)

(defun loc (x l)
  (if (null x) nil
    (CAR (nthcdr x l))
  )
)

(defun run (x y)
  (setf v (nthcdr x y))
  (if (> x (list-length y)) y
    (run (+ 4 x) (store
      (operate (CAR v) (loc (CADR v) y) (loc (CADDR v) y))
      (CADDDR v)
      y))
  )
)

(defun makelist ()
  (loop for i from 0 for j in (CAR (cl-csv:read-csv #P"day2/data2.csv")) collect (parse-integer j))
)

(defun newlist (l noun verb)
  (store verb 2 (store noun 1 l))
)

(defun runX ()
  (ql:quickload :cl-csv)
  (setf l (makelist))
  (loop for i from 0 to 99
    do(loop for j from 0 to 99
      do(
        if (= 19690720 (CAR (run 0 (newlist l i j))))
        (print (+ j (* 100 i)))
      )
    )
  )
)
