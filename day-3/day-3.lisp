(defparameter input 265149)

(defun sub-1-if-even (n)
  (if (evenp n)
	  (1- n)
	  n))

(defun closest-odd-under-square-root (n)
  (sub-1-if-even (floor (sqrt n))))

(defun solve-part-1 (n)
  (let* ((border-entrance-sqrt (closest-odd-under-square-root n))
		 (border-entrance (expt border-entrance-sqrt 2))
		 (edge-index (mod (- n border-entrance)
						  (1+ border-entrance-sqrt)))
		 (middle-index (ceiling (/ border-entrance-sqrt 2)))
		 (distance-to-middle (abs (- edge-index middle-index))))
	(if (eql n border-entrance)
		(1- border-entrance-sqrt)
		(+ middle-index distance-to-middle))))
