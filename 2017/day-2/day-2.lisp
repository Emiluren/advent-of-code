(ql:quickload "split-sequence")

(defun read-input-file ()
  (with-open-file (in (open "input"))
    (loop for line = (read-line in nil)
	   while line collect line)))

(defun split-on-tabs (line)
  (split-sequence:SPLIT-SEQUENCE #\Tab line))

(defun split-lines-on-tabs (lines)
  (mapcar #'split-on-tabs lines))

(defun parse-strings-in-lines (lines)
  (mapcar (lambda (line) (mapcar #'parse-integer line)) lines))

(defun rotate (list-list)
  (apply #'mapcar #'list list-list))

(defun min-and-max (numbers)
  (list (apply #'min numbers) (apply #'max numbers)))

(defun get-nums ()
  (parse-strings-in-lines (split-lines-on-tabs (read-input-file))))

(defun solve-part-1 ()
  (loop for (min max) in (mapcar #'min-and-max (get-nums))
	 sum (- max min)))

(defun possible-index-combos (n)
  (loop for i from 0 below n
	 append (loop for j from 0 below n
			   when (not (eql i j))
			   collect (list i j))))

(defun division-combo (x y)
  (when (eql (mod x y) 0)
	(/ x y)))

(defun possible-divisor (numbers)
  (loop for (i j) in (possible-index-combos (length numbers))
	 thereis (division-combo (elt numbers i) (elt numbers j))))

(defun solve-part-2 ()
  (loop for line in (get-nums)
	 sum (possible-divisor line)))
