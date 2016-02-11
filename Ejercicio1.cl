


;;Ejercicio 2

;;;2.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; newton (f g tol-abs max-iter x0)
;;; Estima el cero de una función mediante Newton_Raphson
;;;
;;; INPUT: f: función cuyo cero se desea encontrar
;;; g: derivada de f
;;; tol-abs: tolerancia para convergencia
;;; max-iter: máximo número de iteraciones
;;; x0: estimación inicial del cero (semilla)
;;;
;;; OUTPUT: estimación del cero de f, o NIL si no converge
;;;

(defun newton (f g tol-abs max-iter x0) 
	
	(if (= max-iter 0) ;Si el maximo de iteraciones es cero devuelve nil
        	nil
        	(if (< (abs (- (setf xn (- x0 (/ (funcion x0) (funcall derivada x0)) )) x0)) tol-abs) ;Comprobamos si en esta iteracion hemos encontrado una raiz de la funcion
             		x0  ;Si hemos encontrado una raiz termina
             		(newton 'f 'g tol-abs (- max-iter 1) xn) ; Si no iteramos una vez mas con el nuevo valor obtenido y reduciendo el max-iter ya que hemos consumido una iteracion
             	)
	)
)

(if (< (abs(- (setf xn (- semilla1 (/ (funcall funcion semilla1) (funcall derivada semilla1)))) semilla1)) tol) 0 1)
(newton funcion derivada tol 50 semilla1)
(un-cero-newton funcion derivada tol iters lst-semillas1)
(newton 'f 'g 1 2 2)


(defun f (x)
       (+ x 1)
)
(defun g (x)
       1
)



(defun suma (x y)
       (+ x y)
)

(defun a (funcion)

       (funcall #'(funcion) 3 3)
      )

(a '(lambda (x y) (+ )



;;;2.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; un-cero-newton (f g tol-abs max-iter semillas)
;;; Prueba con distintas semillas iniciales hasta que Newton
;;; converge
;;;
;;; INPUT: f: función de la que se desea encontrar un cero
;;; g: derivada de f
;;; tol-abs: tolerancia para convergencia
;;; max-iter: máximo número de iteraciones
;;; semillas: semillas con las que invocar a Newton
;;;
;;; OUTPUT: el primer cero de f que se encuentre, o NIL si se diverge
;;; para todas las semillas
;;;
 (defun un-cero-newton (f g tol-abs max-iter semillas)
    (if (equal semillas nil) 
    	nil
    	(if (equal (setf r (newton 'f 'g tol-abs max-iter (pop semillas))) nil) ;El valor que saca el pop se le pasa a newton
	    (un-cero-newton 'f 'g tol-abs max-iter semillas) ;La lista semillas que estamos pasando tiene un elemento menos, el primero que saca el pop y ya se ha probado
	    r
	)    
    )
 )

;Para probar
(un-cero-newton 'f 'g 1 2 '(22 9 3 4 8 6))


(setf tol 1e-6)
>> (setf iters 50)
>> (setf funcion (lambda (x) (* (- x 4) (- x 1) (+ x 3)))) ; raíces: 4, 1, -3
>> (setf derivada (lambda (x) (- (* x (- (* x 3) 4)) 11))) ; 3x^2 - 4x – 11
>> (setf semilla1 2.35287527)
>> (setf semilla2 2.35284172)
>> (setf semilla3 2.352836323)
>> (setf lst-semillas1 (list semilla1 semilla2 semilla3))
>> (setf lst-semillas2 (list semilla2 semilla3 semilla1))
>> (setf lst-semillas3 (list semilla3 semilla1 semilla2))
>> (newton funcion derivada tol iters semilla1)
4.0
>> (newton funcion derivada tol iters semilla2)
-3.0
>> (newton funcion derivada tol iters semilla3)
1.0
>> (un-cero-newton funcion derivada tol iters lst-semillas1)
4.0
>> (un-cero-newton funcion derivada tol iters lst-semillas2)
-3.0
>> (un-cero-newton funcion derivada tol iters lst-semillas3)
1.0
>> (todos-ceros-newton funcion derivada tol iters lst-semillas1)
(4.0 -3.0 1.0)
>> (todos-ceros-newton funcion derivada tol iters lst-semillas2)
(-3.0 1.0 4.0)
>> (todos-ceros-newton funcion derivada tol iters lst-semillas3)
(1.0 4.0 -3.0)










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; todos-ceros-newton (f g tol-abs max-iter semillas)
;;; Prueba con distintas semillas iniciales y devuelve las raíces
;;; encontradas por Newton para dichas semillas
;;;
;;; INPUT: f: función de la que se desea encontrar un cero
;;; g: derivada de f
;;; tol-abs: tolerancia para convergencia
;;; max-iter: máximo número de iteraciones
;;; semillas: semillas con las que invocar a Newton
;;;
;;; OUTPUT: todas las raíces que se encuentren, o NIL si se diverge
;;; para todas las semillas
;;;
(defun todos-ceros-newton (f g tol-abs max-iter semillas)
       (if (equal semillas nil) 
       	   nil
      	   (if (equal (append r (newton 'f 'g tol-abs max-iter (pop semillas))) nil) ;El valor que saca el pop se le pasa a newton
      	 	(un-cero-newton 'f 'g tol-abs max-iter semillas) ;La lista semillas que estamos pasando tiene un elemento menos, el primero que saca el pop y ya se ha probado
      		r
	   )    
       )
)