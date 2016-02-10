


(defun newton (f g tol-abs max-iter x0) 
	
	(if (or (= max-iter 0) (< (abs (- xn xn1)) tol-abs))
		(newton f g  tol-abs max-iter 
			(- x0 
				(/ (f x0) (g x0)))
		(nil)
	)


)