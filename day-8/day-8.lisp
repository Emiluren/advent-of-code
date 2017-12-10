(ql:quickload "split-sequence")

(defun read-input-file ()
  (with-open-file (in (open "input"))
    (loop for line = (read-line in nil)
	   while line collect line)))

(defun split-on-spaces (line)
  (split-sequence:SPLIT-SEQUENCE #\Space line))

(defun split-lines-on-spaces (lines)
  (mapcar #'split-on-spaces lines))

(defun comparison-holds (v1 op v2)
  (case op
	(== (eql v1 v2))
	(>= (>= v1 v2))
	(<= (<= v1 v2))
	(!= (not (eql v1 v2)))
	(> (> v1 v2))
	(< (< v1 v2))))

(defun solve-part-1 ()
  (let ((table (make-hash-table :test 'equal))
		(instructions (split-lines-on-spaces (read-input-file))))
	(loop for (var op amount nil cmp-var cmp-op cmp-val) in instructions
	   do (when (comparison-holds
				 (gethash cmp-var table 0)
				 (intern cmp-op)
				 (parse-integer cmp-val))
			(if (string= "inc" op)
				(incf (gethash var table 0) (parse-integer amount))
				(decf (gethash var table 0) (parse-integer amount)))))
	(loop for key being the hash-keys of table
	   maximize (gethash key table))))

(defun solve-part-2 ()
  (let ((table (make-hash-table :test 'equal))
		(instructions (split-lines-on-spaces (read-input-file))))
	(loop for (var op amount nil cmp-var cmp-op cmp-val) in instructions
	   maximize (gethash var table 0)
	   do (when (comparison-holds
				 (gethash cmp-var table 0)
				 (intern cmp-op)
				 (parse-integer cmp-val))
			(if (string= "inc" op)
				(incf (gethash var table 0) (parse-integer amount))
				(decf (gethash var table 0) (parse-integer amount)))))))
