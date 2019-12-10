(load "day10/day10-1.lisp")
(defvar res)

(defun getline2 (c1 c2)
  (cond ((equal c1 c2) nil)
        ((equal (CAR c1) (CAR c2)) -99)
        ((equal (CDR c1) (CDR c2)) 0)
        (t (/ (- (CDR c2) (CDR c1)) (- (CAR c2) (CAR c1))))))

(defun ingrad (c1 c2 g)
  (equal (getline2 c1 c2) g))

(defun lines2 (c1 l)
  (remove-duplicates (loop for c in l if (getline2 c1 c) collect (getline2 c1 c)) :test `equal))

(defun listpath (x l gl)
  (loop for g in gl collect (loop for c in l if (ingrad x c g) collect c)))

(defun killasteroid (l n x d count)
  (cond ((= count 200) (- n 1))
        ((= n (list-length l)) (killasteroid l 0 x (not d) count))
        ((null (nth n l)) (killasteroid l (+ n 1) x d count))
        ((NOT (null (validkill (nth n l) x d))) (setq res (nth (validkill (nth n l) x d) (nth n l)))
                                                (setq l (eliminate n l (validkill (nth n l) x d)))
                                                (killasteroid l (+ n 1) x d (+ count 1)))
        (t (killasteroid l (+ n 1) x d count))))

(defun eliminate (n l p)
  (loop for i from 0 for c in l collect (if (= i n) (remove (nth p c) c :test `equal) c)))

(defun validkill (l x d)
  (setq l2 (mapcar `CAR l))
  (cond (d (position t (mapcar (lambda (y) (>= (- y (CAR x)) 0)) l2)))
        (t (position t (mapcar (lambda (y) (<= (- y (CAR x)) 0)) l2)))))

(defun run2 ()
  (setq l (coordlist (getlist)))
  (setq l2 (loop for c in l collect (lines2 c l)))
  (setq f (loop for c in l collect (lines c l)))
  (setq x (nth (position (apply `max f) f) l))
  (setq z (listpath x l (sort (nth (position (apply `max f) f) l2) `<)))
  (setq y (killasteroid z 0 x t 0))
  res)
