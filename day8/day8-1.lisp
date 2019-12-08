(defvar image)

(defun split (x)
 (when (not (null x)) (push (loop for i from 0 to 5 collect
                              (loop for j from 0 to 24 collect (pop x))) image) (split x)))

(defun layercount (target)
  (loop for l in image collect (apply `+
    (loop for r in l collect
      (loop for c in r count (if (= c target) t nil))))))

(defun getminpos ()
  (position (apply `min (layercount 0)) (layercount 0)))

(defun getimage ()
  (parse-integer (CAAR (cl-csv:read-csv #P"day8/data8.csv"))))

(defun number-to-list (n)
  (loop for c across (write-to-string n) collect (digit-char-p c)))

(defun run ()
  (setq image `())
  (split (number-to-list (getimage)))
  (setq pos (getminpos))
  (* (nth pos (layercount 1)) (nth pos (layercount 2))))
