(defun fuel (y)
  (- (floor(/ y 3)) 2)
)

(defun getfuel (x)
  (apply `+ (mapcar `fuel x))
)

(defun run ()
  (load "getfile.lisp")
  (getfuel (getfile "day1/data1.txt"))
)
