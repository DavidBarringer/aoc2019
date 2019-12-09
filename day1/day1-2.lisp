(defun negativeloop (y)
  (if (> (- (floor (/ y 3)) 2) 0)
    (+ y (negativeloop(- (floor(/ y 3)) 2))) (+ y 0)))

(defun getfuel (x)
  (apply `+ (mapcar `- (mapcar `negativeloop x) x)))

(defun run ()
  (load "../getfile.lisp")
  (getfuel (getfile "data1.txt")))
