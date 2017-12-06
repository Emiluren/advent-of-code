(ql:quickload "split-sequence")

(defun read-input-data ()
  (with-open-file (in (open "input"))
	(mapcar #'parse-integer
			(split-sequence:SPLIT-SEQUENCE #\Tab (read-line in)))))

(defun calc-new-state (state)
  (let* ((max-val (apply #'max state))
		 (max-index (position max-val state))
		 (new-state (copy-list state)))
	(setf (elt new-state max-index) 0)
	(loop
	   for i from 1 to max-val
	   for j = (mod (+ max-index i) (length state))
	   do (incf (elt new-state j)))
	new-state))

(defun necessary-steps (data)
  (let ((states (list data)))
	(loop for new-state = (calc-new-state (car states))
	   while (not (member new-state states :test #'equal))
	   do (push new-state states))
	states))

(defun solve-part-1 ()
  (length (necessary-steps (read-input-data))))

(defun solve-part-2 ()
  (let* ((states (necessary-steps (read-input-data)))
		 (repeated-state (calc-new-state (car states))))
	(1+ (position repeated-state states :test #'equal))))
