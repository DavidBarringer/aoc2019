(ql:quickload :cl-csv)
(ql:quickload :cl-ppcre)

(defun numtolist (n)
  (loop for c across (write-to-string n) collect (digit-char-p c)))

(defun tonum (x)
  (parse-integer (format nil "~{~A~}" x)))
