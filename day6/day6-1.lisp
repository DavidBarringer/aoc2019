(defvar chart)
(defvar orbitalsum)
(defvar l)

(defun makelist ()
  (setq l (loop for c in (cl-csv:read-csv #P"day6/data6.csv") collect (cl-ppcre:split "\\)" (CAR c))))
)

(defun maketree (test)
  (setq x (loop for c in l when(equal (CAR c) test) collect (CDR c)))
  (loop for c in x do (addtotree test (CAR c))))

(defun addtotree (test c)
  (setq chart (addchild test chart (makeorbital c) 1))
  (maketree c))

(defun makeorbital (name)
  (cons (cons name nil) nil))

(defun addorbital (m orbital)
  (append m orbital))

(defun getchild (m i)
  (CAR (nthcdr i m)))

(defun addchild (test m val depth)
  (cond ((atom m) m)
        ((equal (CAR m) test) (incf orbitalsum depth) (addorbital m val))
        (t (remove nil (loop for i from 0 to (list-length m) collect (addchild test (getchild m i) val (+ depth 1)))))))

(defun run ()
  (setq orbitalsum 0)
  (setq chart `("COM"))
  (makelist)
  (maketree "COM")
  orbitalsum)
