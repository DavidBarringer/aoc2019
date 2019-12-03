(defun check (coord l)
  (if (OR
      (AND (< (CAAR l) (CAR coord)) (> (CAADR l) (CAR coord)) (= (CDAR l) (CDR coord)))
      (AND (> (CAAR l) (CAR coord)) (< (CAADR l) (CAR coord)) (= (CDAR l) (CDR coord)))
      (AND (> (CDAR l) (CDR coord)) (< (CDADR l) (CDR coord)) (= (CAAR l) (CAR coord)))
      (AND (< (CDAR l) (CDR coord)) (> (CDADR l) (CDR coord)) (= (CAAR l) (CAR coord)))
  ) T Nil)
)

(defun getSum (sum coord l)
  (if (= (list-length l) 1) (print "BIG ERROR")
    (if (null (check coord l))
      (getSum (+ sum (+ (abs(- (CAAR l) (CAADR l))) (abs(- (CDAR l) (CDADR l))))) coord (CDR l))
      (+ sum (abs (- (CAAR l) (CAR coord))) (abs (- (CDAR l) (CDR coord))))
    )
  )
)

(defun sumlist (crosslist l res)
  (if (null crosslist) res
    (sumlist (CDR crosslist) l (append res (list (getsum 0 (CAR crosslist) l))))
  )
)

(defun run2 ()
  (load "day3/day3-1.lisp")
  (apply `min (mapcar `+
  (sumlist (checkcrossing (coord (CAR (getWires)) `((0 . 0))) (coord (CADR (getWires)) `((0 . 0))) ()) (coord (CADR (getWires)) `((0 . 0))) ())
  (sumlist (checkcrossing (coord (CAR (getWires)) `((0 . 0))) (coord (CADR (getWires)) `((0 . 0))) ()) (coord (CAR (getWires)) `((0 . 0))) ())))
)
