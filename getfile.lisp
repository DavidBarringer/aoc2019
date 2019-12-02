(defun getfile (path)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
          while line
          collect (parse-integer line))))
