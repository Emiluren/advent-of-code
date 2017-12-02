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




