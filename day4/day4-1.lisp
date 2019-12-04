(defun number-sequence (s e)
  (loop for i from s to e collect (list i))
)

(defun generate (l x)
  (if (= l 1) (number-sequence x 9)
    (loop for i from x to 9 append (
      loop for d in (generate (- l 1) i) collect (cons i d)))
  )
)

(defun tonum (l)
  (parse-integer (format nil "~{~A~}" l))
)

(defun inrange (c r)
  (AND (> (tonum c) (parse-integer (CAAR r))) (< (tonum c) (parse-integer (CADAR r))))
)

(defun consecutive (x tot)
  (if (= (list-length x) 1) (list tot)
    (if (= (CAR x) (CADR x)) (consecutive (CDR x) (+ tot 1)) (append (list tot) (consecutive (CDR x) 1)))
  )
)

(defun run ()
  (setf r (cl-csv:read-csv #P"day4/data4.csv"))
  (loop for c in (generate (length (CADAR r)) 1) count (if (AND (apply `<= c) (find `1 (consecutive c 1) :test `<) (inrange c r)) c))
)
