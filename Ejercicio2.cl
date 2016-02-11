


(defun newton (f g tol-abs max-iter x0) 
	
	(if (or (= max-iter 0) (< (abs (- xn xn1)) tol-abs))
		(newton f g  tol-abs max-iter 
			(- x0 
				(/ (f x0) (g x0)))
		(nil)
	)


)









(defun newton (f g tol-abs max-iter x0) 
	
	(if (= (setf max-iter (- max-iter 1)) -1) )
        	(nil)
        	(if (< (- (setf xn (- x0 (/ (funcall f x0) (funcall g x0) ))) x0) tol-abs)
             		x0
             		(newton f g tol-abs max-iter xn)
             	)
	)
)


(defun f (x)
  (+ x 1)
)

(defun g (x)
  1)

(newton f g 50 10 2)
