(defun pathto (test m path)
  (cond ((atom m) nil)
        ((equal (CAR m) test) path)
        (t (loop for i from 0 to (list-length m) append (pathto test (getchild m i) (append path (list (CAR m))))))))

(defun diff (x y)
  (list-length (set-difference (pathto x chart `()) (pathto y chart `()))))

(defun run2 ()
  (load "day6/day6-1.lisp")
  (run)
  (+ (diff "SAN" "YOU") (diff "YOU" "SAN")))
