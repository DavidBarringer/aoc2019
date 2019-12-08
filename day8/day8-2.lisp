(defvar image)

(defun split (x)
 (when (not (null x)) (push (loop for i from 0 to 5 collect
                              (loop for j from 0 to 24 collect (pop x))) image) (split x)))

(defun getpixel (l r c)
  (setq pixel (nth c (nth r (nth l image))))
  (cond ((= pixel 0) "█")
        ((= pixel 1) " ")
        ((= pixel 2) (getpixel (+ l 1) r c))))

(defun getimage ()
  (parse-integer (CAAR (cl-csv:read-csv #P"day8/data8.csv"))))

(defun number-to-list (n)
  (loop for c across (write-to-string n) collect (digit-char-p c)))

(defun tostring (lst)
    (format nil "~%~{~A~}" lst))

(defun run2 ()
  (setq image `())
  (split (number-to-list (getimage)))
  (setq image (reverse image))
  (tostring (loop for r from 0 to 5 collect
    (tostring (loop for c from 0 to 24 collect (getpixel 0 r c))))))
