(defparameter *a-test-start* 65)
(defparameter *b-test-start* 8921)

(defparameter *a-factor* 16807)
(defparameter *b-factor* 48271)

(defparameter *divisor* 2147483647)

(defun next-value (old-value factor)
  (mod (* old-value factor) *divisor*))

(defun generate-n-values (start-value factor n)
  (loop for value = (next-value start-value factor)
	 then (next-value value factor)

	 for i from 1 to n
	 collect value))

(defun mask-16-lowest (n)
  (mask-field (byte 16 0) n))

(defun same-lower-bits-for-n (n a-generator b-generator)
  (loop for i from 1 to n

	 for a-value = (funcall a-generator)
	 then (funcall a-generator a-value)

	 for b-value = (funcall b-generator)
	 then (funcall b-generator b-value)

	 count (eql (mask-16-lowest a-value)
				(mask-16-lowest b-value))))

(defun part-1-a-generator (&optional old-value)
  (if old-value
	(next-value old-value *a-factor*)
	(next-value 679 *a-factor*)))

(defun part-1-b-generator (&optional old-value)
  (if old-value
	(next-value old-value *b-factor*)
	(next-value 771 *b-factor*)))

(defun solve-part-1 ()
  (same-lower-bits-for-n 40000000
						 #'part-1-a-generator
						 #'part-1-b-generator))

(defun run-until-divisible-by (generator divisor &optional old-value)
  (loop for val = (funcall generator old-value)
	 then (funcall generator val)

	 until (eql (mod val divisor) 0)
	 finally (return val)))

(defun part-2-a-generator (&optional old-value)
  (run-until-divisible-by #'part-1-a-generator 4 old-value))

(defun part-2-b-generator (&optional old-value)
  (run-until-divisible-by #'part-1-b-generator 8 old-value))

(defun solve-part-2 ()
  (same-lower-bits-for-n 5000000
						 #'part-2-a-generator
						 #'part-2-b-generator))
