(defun run2 ()
  (load "day4/day4-1.lisp")
  (setf r (cl-csv:read-csv #P"day4/data4.csv"))
  ; basically the same as part one, but find only succeeds if a 2 is present in consecutive
  (loop for c in (generate (length (CADAR r)) 1) count (if (AND (find `2 (consecutive c 1)) (inrange c r)) c)))
