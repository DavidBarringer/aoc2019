(defun manhattan (x)
  (+ (abs (CAR x)) (abs(CDR x))))

; gets direction, applies it to last co-ordinate and puts it at the end of the pivot point list
; this is done in a very inefficient way by past me, past me didn't know better
(defun coord (path l)
  (if (null path) l
    (cond ((eql (char (CAR path) 0) #\L)
            (coord (CDR path) (append l (list (CONS (- (CAAR (last l 1))
                                                       (parse-integer (CAR path) :start 1)) (CDAR (last l 1)))))))
          ((eql (char (CAR path) 0) #\R)
            (coord (CDR path) (append l (list (CONS (+ (CAAR (last l 1))
                                                       (parse-integer (CAR path) :start 1)) (CDAR (last l 1)))))))
          ((eql (char (CAR path) 0) #\U)
            (coord (CDR path) (append l (list (CONS (CAAR (last l 1))
                                                    (+ (CDAR (last l 1)) (parse-integer (CAR path) :start 1)))))))
          ((eql (char (CAR path) 0) #\D)
            (coord (CDR path) (append l (list (CONS (CAAR (last l 1))
                                                    (- (CDAR (last l 1)) (parse-integer (CAR path) :start 1))))))))))

; checks that the pivot points create an intersection
; this is done by checking that the fixed axis of two points falls between the varying axis of the others
(defun intersect (c1 c2 p1 p2)
  (AND (OR (AND (> (CAR c1) (CAR p1)) (< (CAR c2) (CAR p2))) (AND (< (CAR c1) (CAR p1)) (> (CAR c2) (CAR p2))))
       (OR (AND (> (CDR c1) (CDR p1)) (< (CDR c2) (CDR p2))) (AND (< (CDR c1) (CDR p1)) (> (CDR c2) (CDR p2))))))

(defun crossing (c1 c2 clist res)
  (if (= (list-length clist) 1) res
    (if (= (CAR c1) (CAR c2)) ; x points don't change
      (if (intersect c1 c2 (CAR clist) (CADR clist))
          (crossing c1 c2 (CDR clist) (append res (list(CONS (CAR c1) (CDAR clist)))))
          (crossing c1 c2 (CDR clist) res))
      ; y points don't change
      (if (intersect c1 c2 (CAR clist) (CADR clist))
          (crossing c1 c2 (CDR clist) (append res (list(CONS (CAAR clist) (CDR c1)))))
          (crossing c1 c2 (CDR clist) res)))))

; goes through each pair of points of one wire and compares against each pair of the other wire
(defun checkCrossing (l1 l2 res)
  (if (= (list-length l1) 1) res
    (checkCrossing (CDR l1) l2 (crossing (CAR l1) (CADR l1) l2 res))))

(defun getWires ()
  (cl-csv:read-csv #P"day3/data3.csv"))

(defun run ()
  (apply `min (mapcar `manhattan (checkcrossing (coord (CAR (getWires)) `((0 . 0)))
                                                (coord (CADR (getWires)) `((0 . 0))) `()))))
