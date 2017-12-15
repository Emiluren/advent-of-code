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

(defun same-lower-bits-for-n (n a-start b-start)
  (loop for i from 1 to n

	 for a-value = (next-value a-start *a-factor*)
	 then (next-value a-value *a-factor*)

	 for b-value = (next-value b-start *b-factor*)
	 then (next-value b-value *b-factor*)

	 count (eql (mask-16-lowest a-value) (mask-16-lowest b-value))))

(defun solve-part-1 ()
  (same-lower-bits-for-n 40000000 679 771))
