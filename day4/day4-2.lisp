(defun run2 ()
  (load "day4/day4-1.lisp")
  (setf r (cl-csv:read-csv #P"day4/data4.csv"))
  (loop for c in (range (CAAR r) (CADAR r)) count (if (AND (isascending c) (find `2 (consecutive c 1))) c))
)
