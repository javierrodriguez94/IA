(defconstant +bicond+ '<=>)
(defconstant +cond+ '=>)
(defconstant +and+ '^)
(defconstant +or+ 'v)
(defconstant +not+ '¬)

(defun truth-value-p (x)
	(or (eql x T) (eql x NIL)))
(defun unary-connector-p (x)
	(eql x +not+))
(defun binary-connector-p (x)
	(or (eql x +bicond+)
	(eql x +cond+)))
(defun n-ary-connector-p (x)
	(or (eql x +and+)
	(eql x +or+)))
(defun connector-p (x)
	(or (unary-connector-p x)
	(binary-connector-p x)
	(n-ary-connector-p x)))

(defun remove-parenthesis (expr)
    (if (null expr)
        nil
        (if (atom (first expr))
        	(cons (first expr) (remove-parenthesis (rest expr)))
        	(append (remove-parenthesis (first expr)) (remove-parenthesis (rest expr)))
        )
    )
)



(defun extrae-simbolos (expr) 
	(remove-duplicates 
		(remove-if #'connector-p
			(remove-if #'truth-value-p
				(remove-parenthesis expr)
			)
		)
	)
)
