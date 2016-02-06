; (= (length x) (length y))

(defun lp-rec (x y p)
  (expt (lp-rec-aux x y p) 
        (/ 1 p)))

(defun lp-rec-aux (x y p)
  (if (null x)
      0
    (+ (expt (abs (- (first x) (first y))) p) 
       (lp-rec-aux (rest x) (rest y) p))))

(lp-rec '(1 3 6) '(2 4 1) 2)


(defun lp-mapcar (x y p)
  (expt (reduce #'+ (mapcar #'(lambda (a b)
                                (expt (abs (- a b)) p))
                      x 
                      y))
        (/ 1 p)))
  
(lp-mapcar '(1 3 6) '(2 4 1) 2)