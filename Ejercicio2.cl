


(defun newton (f g tol-abs max-iter x0) 
	
	(if (or (= max-iter 0) (< (abs (- xn xn1)) tol-abs))
		(newton f g  tol-abs max-iter 
			(- x0 
				(/ (f x0) (g x0)))
		(nil)
	)


)









(defun newton (f g tol-abs max-iter x0) 
	
	(if (or (= (setf max-iter (- max-iter 1)) 0) (= 1 0))
         (if (= x0 (setf xn (- x0 (/ (f x0) (g x0) ))))
             x0
             (newton f g tol-abs xn)
             )
     
		(nil)
	)
)




(defun g (x)
  1)

(newton f g 50 10 2)
