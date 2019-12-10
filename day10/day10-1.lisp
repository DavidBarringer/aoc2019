(defun getline (c1 c2)
  (cond ((equal c1 c2) nil)
        ((AND (equal (CAR c1) (CAR c2)) (< (CDR c1) (CDR c2))) (concatenate `string "x=" (format nil "~A.1" (CAR c1))))
        ((AND (equal (CAR c1) (CAR c2)) (> (CDR c1) (CDR c2))) (concatenate `string "x=" (format nil "~A.2" (CAR c1))))
        ((AND (equal (CDR c1) (CDR c2)) (< (CAR c1) (CAR c2))) (concatenate `string "y=" (format nil "~A.1" (CDR c1))))
        ((AND (equal (CDR c1) (CDR c2)) (> (CAR c1) (CAR c2))) (concatenate `string "y=" (format nil "~A.2" (CDR c1))))
        ((< (CDR c1) (CDR c2)) (cons 1 (/ (- (CDR c2) (CDR c1)) (- (CAR c2) (CAR c1)))))
        ((> (CDR c1) (CDR c2)) (cons 2 (/ (- (CDR c2) (CDR c1)) (- (CAR c2) (CAR c1)))))))

(defun coordrow (l x y res)
  (cond ((null l) res)
        ((equal (CAR l) #\#) (coordrow (CDR l) (+ x 1) y (push (cons x y) res)))
        (t (coordrow (CDR l) (+ x 1) y res))))

(defun coordlist (l)
  (loop for i from 0 for c in l append (reverse (coordrow (concatenate `list (CAR c)) 0 i nil))))

(defun lines (c1 l)
  (list-length (remove-duplicates (loop for c in l if (getline c1 c) collect (getline c1 c)) :test `equal)))

(defun getlist ()
  (cl-csv:read-csv #P"day10/data10.csv"))

(defun run ()
  (setq l (coordlist (getlist)))
  (apply `max (loop for c in l collect (lines c l))))
