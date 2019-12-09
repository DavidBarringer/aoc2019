(defvar image)

(defun split (x)
 (when (not (null x)) (push (loop for i from 0 to 5 collect
                              (loop for j from 0 to 24 collect (pop x))) image) (split x)))

; counts each occurence of target in a row, then sums the result for each row
(defun layercount (target)
  (loop for l in image collect (apply `+
    (loop for r in l collect
      (loop for c in r count (if (= c target) t nil))))))

(defun getminpos ()
  (position (apply `min (layercount 0)) (layercount 0)))

(defun getimage ()
  (parse-integer (CAAR (cl-csv:read-csv #P"day8/data8.csv"))))

(defun run ()
  (setq image `())
  (split (numtolist (getimage)))
  (setq pos (getminpos)) ; sets pos to layer with smallest number of 0's
  (* (nth pos (layercount 1)) (nth pos (layercount 2))))
