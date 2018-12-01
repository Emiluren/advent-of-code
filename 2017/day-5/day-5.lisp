(defun read-input-file ()
  (with-open-file (in (open "input"))
    (loop for line = (read-line in nil)
	   while line collect line)))

(defun parse-strings-in-lines (lines)
  (mapcar #'parse-integer lines))

(defparameter input (parse-strings-in-lines (read-input-file)))

(defun steps-to-get-out (step-list offset-adder-f)
  (let ((data (make-array
			   (length step-list)
			   :initial-contents step-list))
		(program-counter 0)
		(steps 0))
	(loop while (< program-counter (length data))
	   do (progn
			(let ((old-pc program-counter))
			  (incf program-counter (elt data program-counter))
			  (incf (elt data old-pc)
					(funcall offset-adder-f (elt data old-pc))))
			(incf steps)))
	steps))

(defun solve-part-1 ()
  (steps-to-get-out input
					(lambda (x)
					  (declare (ignore x))
					  1)))

(defun solve-part-2 ()
  (steps-to-get-out input
					(lambda (x)
					  (if (>= x 3) -1 1))))
