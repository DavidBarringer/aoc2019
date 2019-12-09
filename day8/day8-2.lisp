(defvar image)

(defun split (x)
 (when (not (null x)) (push (loop for i from 0 to 5 collect
                              (loop for j from 0 to 24 collect (pop x))) image) (split x)))

; gets value for given layer, if transparent run again for next layer
(defun getpixel (l r c)
  (setq pixel (nth c (nth r (nth l image))))
  (cond ((= pixel 0) " ")
        ((= pixel 1) "█")
        ((= pixel 2) (getpixel (+ l 1) r c))))

(defun getimage ()
  (parse-integer (CAAR (cl-csv:read-csv #P"day8/data8.csv"))))

(defun tostring (lst)
    (format nil "~%~{~A~}" lst))

(defun run2 ()
  (setq image `())
  (split (numtolist (getimage)))
  (setq image (reverse image))
  (tostring (loop for r from 0 to 5 collect
    (tostring (loop for c from 0 to 24 collect (getpixel 0 r c))))))
