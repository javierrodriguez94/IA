

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

; 1.2
(defun l2-rec (x y)
  (lp-rec x y 2))

(l2-rec '(1 3 6) '(2 4 1))

(defun l2-mapcar (x y)
  (lp-mapcar x y 2))

(l2-mapcar '(1 3 6) '(2 4 1))

(defun l1-rec (x y)
  (lp-rec x y 1))

(l1-rec '(1 3 6) '(2 4 1))

(defun l1-mapcar (x y)
  (lp-mapcar x y 1))

(l1-mapcar '(1 3 6) '(2 4 1))

(defun nearest (vector vectors distance)
(unless (null vectors)
(reduce
#'(lambda (x y)
(if (< (funcall distance vector x)
(funcall distance vector y))
x
y)) vectors)))

(setf vectors '((0.1 0.1 0.1) (0.2 -0.1 -0.1)))
(nearest '(1.0 -2.0 3.0) vectors #'l2-mapcar)
(nearest '(1.0 -2.0 3.0) vectors #'l1-rec)
;time measure
(time (nearest '(1.0 -2.0 3.0) vectors #'l1-rec))
(time (nearest '(1.0 -2.0 3.0) vectors #'l2-mapcar))

(setf bigVectors '((0.1 0.1 0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1) (0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1) (0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1)))

(setf superBigVectors (append bigVectors bigVectors bigVectors bigVectors bigVectors bigVectors bigVectors bigVectors bigVectors bigVectors bigVectors bigVectors bigVectors bigVectors bigVectors bigVectors bigVectors bigVectors bigVectors bigVectors bigVectors bigVectors bigVectors))
superBigVectors

(setf hiperSuperBigVectors (append superBigVectors superBigVectors superBigVectors superBigVectors superBigVectors superBigVectors superBigVectors superBigVectors superBigVectors superBigVectors superBigVectors superBigVectors superBigVectors superBigVectors superBigVectors superBigVectors superBigVectors))
hiperSuperBigVectors

(time (nearest '(0.1 0.1 0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1) superBigVectors #'l1-rec))

(time (nearest '(0.1 0.1 0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1) hiperSuperBigVectors #'l1-rec))

(time (nearest '(0.1 0.1 0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1) superBigVectors #'l1-mapcar))

(time (nearest '(0.1 0.1 0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1) hiperSuperBigVectors #'l1-mapcar))

(time (nearest '(0.1 0.1 0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1) superBigVectors #'l2-rec))

(time (nearest '(0.1 0.1 0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1) hiperSuperBigVectors #'l2-rec))

(time (nearest '(0.1 0.1 0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1) superBigVectors #'l2-mapcar))

(time (nearest '(0.1 0.1 0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1) hiperSuperBigVectors #'l2-mapcar))


;;CG-USER(41): 
; cpu time (non-gc) 0.015600 sec user, 0.000000 sec system
; cpu time (gc)     0.031201 sec user, 0.000000 sec system
; cpu time (total)  0.046801 sec user, 0.000000 sec system
; real time  0.053000 sec ( 88.3%)
; space allocation:
;  159,585 cons cells, 3,356,856 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
;(0.1 0.1 0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 ...)
; cpu time (non-gc) 0.280802 sec user, 0.000000 sec system

;;CG-USER(42): 
; cpu time (non-gc) 0.280802 sec user, 0.000000 sec system
; cpu time (gc)     0.109200 sec user, 0.000000 sec system
; cpu time (total)  0.390002 sec user, 0.000000 sec system
; real time  0.394000 sec (98.99%)
; space allocation:
;  2,749,569 cons cells, 57,825,816 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
;(0.1 0.1 0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 ...)
;;CG-USER(43): 
; cpu time (non-gc) 0.031201 sec user, 0.000000 sec system
; cpu time (gc)     0.015600 sec user, 0.000000 sec system
; cpu time (total)  0.046801 sec user, 0.000000 sec system
; real time  0.043000 sec (108.8%)
; space allocation:
;  94,713 cons cells, 3,274,168 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
;(0.1 0.1 0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 ...)
; cpu time (non-gc) 0.218401 sec user, 0.000000 sec system

;;CG-USER(44): 
; cpu time (non-gc) 0.218401 sec user, 0.000000 sec system
; cpu time (gc)     0.093601 sec user, 0.000000 sec system
; cpu time (total)  0.312002 sec user, 0.000000 sec system
; real time  0.314000 sec (99.36%)
; space allocation:
;  1,631,481 cons cells, 56,400,376 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
;(0.1 0.1 0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 ...)
;;CG-USER(45): 
; cpu time (non-gc) 0.031201 sec user, 0.000000 sec system
; cpu time (gc)     0.015600 sec user, 0.000000 sec system
; cpu time (total)  0.046801 sec user, 0.000000 sec system
; real time  0.052000 sec ( 90.0%)
; space allocation:
;  159,585 cons cells, 3,374,376 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
;(0.1 0.1 0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 ...)
; cpu time (non-gc) 0.358802 sec user, 0.000000 sec system

;;CG-USER(46): 
; cpu time (non-gc) 0.358802 sec user, 0.000000 sec system
; cpu time (gc)     0.062400 sec user, 0.000000 sec system
; cpu time (total)  0.421202 sec user, 0.000000 sec system
; real time  0.424000 sec (99.34%)
; space allocation:
;  2,749,569 cons cells, 58,125,384 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
;(0.1 0.1 0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 ...)
;;CG-USER(47): 
; cpu time (non-gc) 0.031201 sec user, 0.000000 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  0.031201 sec user, 0.000000 sec system
; real time  0.043000 sec (72.56%)
; space allocation:
;  94,713 cons cells, 3,291,784 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
;(0.1 0.1 0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 ...)
; cpu time (non-gc) 0.265202 sec user, 0.000000 sec system

;;CG-USER(48): 
; cpu time (non-gc) 0.265202 sec user, 0.000000 sec system
; cpu time (gc)     0.078000 sec user, 0.000000 sec system
; cpu time (total)  0.343202 sec user, 0.000000 sec system
; real time  0.343000 sec (100.1%)
; space allocation:
;  1,631,481 cons cells, 56,700,712 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
;(0.1 0.1 0.1 0.2 -0.1 -0.1 0.2 -0.1 -0.1 0.2 ...)

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
        	(if (< (abs (- (setf xn (- x0 (/ (funcall 'f x0) (funcall 'g x0)))) x0)) tol-abs) ;Comprobamos si en esta iteracion hemos encontrado una raiz de la funcion
             		x0 ;Si hemos encontrado una raiz termina
             		(newton 'f 'g tol-abs (- max-iter 1) xn) ; Si no iteramos una vez mas con el nuevo valor obtenido y reduciendo el max-iter ya que hemos consumido una iteracion
             	)
	)
)

(newton 'f 'g 1 2 2)


(defun f (x)
       (+ x 1)
)
(defun g (x)
       1
)


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

;2.3
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



;;Ejercicio 3

;3.1
(defun combine-elt-lst (elt lst)
  (if (null elt)
      nil
    (mapcar #'(lambda (x) (list elt x)) lst)))

;;3.2
(defun combine-lst-lst (lst1 lst2)
  (if (or (null lst1) (null lst2))
      nil
    (append (combine-elt-lst (first lst1) lst2) (combine-lst-lst (rest lst1) lst2))))

;;3.3
;;Auxiliar 1
(defun combine-elt-lstolsts (elt lstolsts)
  (if (null lstolsts)
      nil
  (mapcar #'(lambda (x) (append (list elt) x)) lstolsts)))

;;Auxiliar
;;;combina una lista de elementos con una lista de listas uniendo los elementos a las listas de la lista.
(defun combine-list-with-list-of-list (lst lstolsts)
  (if (null lst)
      nil
  ;(let (myfirst (first lst))
  (append (combine-elt-lstolsts (first lst) lstolsts) (combine-list-with-list-of-list (rest lst) lstolsts))))

(defun combine-list-of-lsts (lstolsts)
  (if (null (rest lstolsts))
  (mapcar #'(lambda (x) (list x)) (first lstolsts))
    (combine-list-with-list-of-list (first lstolsts) (combine-list-of-lsts (rest lstolsts)))))

;;Test:
(combine-elt-lst 'a '(1 2 3));->((A 1) (A 2) (A 3))
(combine-elt-lst nil '(1 2 3));->NIL
(combine-elt-lst nil nil);->NIL
(combine-elt-lst '(1 2 3) nil);->NIL

(combine-lst-lst '(a b c) '(1 2 3))
(combine-lst-lst nil '(1 2 3));->NIL
(combine-lst-lst nil nil);->NIL
(combine-lst-lst '(1 2 3) nil);->NIL

(combine-list-of-lsts '((a b c) (1 2 3) (x y z)))
(combine-list-of-lsts '((a b c)(1 2 3)))
(combine-list-of-lsts '((a b c)))
(combine-list-of-lsts nil)
(combine-list-of-lsts '((a b c)nil))
(combine-list-of-lsts '(nil(1 2 3)))
(combine-list-of-lsts '((a b c) nil (x y z)))


(combine-list-with-list-of-list '(a b c) '((1 2 3) (x y z)))

(combine-elt-lstolsts 'a '((1 2 3) (x y z)))

































