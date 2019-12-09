(defun operate (op x y)
  (if (= op 1) (+ x y)
    (if (= op 2) (* x y)
      (if (= op 99) nil))))

; remakes the machine list, replacing the value at given location
(defun store (x loc l)
  (if (null loc) l (loop for i from 0 for j in l collect (if (= i loc) x j))))

(defun loc (x l)
  (if (null x) nil (nth x l)))

; v is used as a pointer for the machine, all operations increase the pointer by 4
; if the pointer is beyond the end of the machine, ends the running of the machine
(defun intcode (p l)
  (setq v (nthcdr p l))
  (if (> p (list-length l)) l
    (intcode (+ 4 p) (store
      (operate (CAR v) (loc (CADR v) l) (loc (CADDR v) l))
      (CADDDR v)
      l))))

; reads the input csv, which produces a list, but the list has the numbers as strings
; so we have to remake the list using parse-integer
(defun makelist ()
  (loop for i from 0 for j in (CAR (cl-csv:read-csv #P"day2/data2.csv")) collect (parse-integer j)))

(defun run ()
  (CAR (intcode 0 (makelist))))
