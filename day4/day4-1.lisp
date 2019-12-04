(defun range (x y)
  (loop for i from (parse-integer x) to (parse-integer y) collect (numbertolist i))
)

(defun numbertolist (n)
  (loop for c across (write-to-string n) collect (digit-char-p c))
)

(defun consecutive (x tot)
  (if (= (list-length x) 1) (list tot)
    (if (= (CAR x) (CADR x)) (consecutive (CDR x) (+ tot 1)) (append (list tot) (consecutive (CDR x) 1)))
  )
)

(defun run ()
  (setf r (cl-csv:read-csv #P"day4/data4.csv"))
  (loop for c in (range (CAAR r) (CADAR r)) count (if (AND (apply `<= c) (find `1 (consecutive c 1) :test `<)) c))
)
