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

(defun get-neighbours (point)
  (destructuring-bind (x . y) point
	(list (cons (1+ x) y)
		  (cons (1- x) y)
		  (cons x (1+ y))
		  (cons x (1- y)))))

(defun value-for-new-point (point table)
  (reduce #'+
		  (mapcar (lambda (p) (gethash p table 0))
				  (get-neighbours point))))

(defparameter *table-init-data*
  '(((0 . 0) . 1)
	;; ((1 . 0) . 1)
	;; ((1 . 1) . 2)
	;; ((0 . 1) . 4)
	;; ((-1 . 1) . 5)
	;; ((-1 . 0) . 10))
  )

(defun init-table ()
  (let ((table (make-hash-table :test 'equal)))
	(loop for (pos . value) in *table-init-data*
	   do (setf (gethash pos table) value))
	table))

(defun solve-part-2 ()
  (let ((table (init-table)))
	(loop
	   for new-value = (value-for-new-point new-point table)
	   do (setf (gethash new-point table) new-value))
	new-value))
