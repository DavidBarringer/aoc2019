(defun number-sequence (s e)
  (loop for i from s to e collect (list i)))

; makes a list of all ascending sequences of length l
; hard to explain, recommend experimenting with generate using low l and x from 9-1
(defun generate (l x)
  (if (= l 1) (number-sequence x 9)
    (loop for i from x to 9 append (
      loop for d in (generate (- l 1) i) collect (cons i d)))))

; changes list c into a number, compares against the input ranges
(defun inrange (c r)
  (AND (> (tonum c) (parse-integer (CAAR r))) (< (tonum c) (parse-integer (CADAR r)))))

; if the next number in the list is the same as the current one, add 1 to total and check the next one
; otherwise, store previous total and start counting from the next number
(defun consecutive (x tot)
  (if (= (list-length x) 1) (list tot)
    (if (= (CAR x) (CADR x)) (consecutive (CDR x) (+ tot 1)) (append (list tot) (consecutive (CDR x) 1)))))

(defun run ()
  (setf r (cl-csv:read-csv #P"day4/data4.csv"))
  ; counts all non-nil values i.e. each c that passes the conditions
  (loop for c in (generate (length (CADAR r)) 1) count
    (if (AND (find `1 (consecutive c 1) :test `<) (inrange c r)) c)))
