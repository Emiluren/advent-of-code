(defun digits (num digs)
  (if (< num 10)
      (cons num digs)
      (multiple-value-bind (quot rem) (floor num 10)
        (digits quot (cons rem digs)))))

(defun group-lengths (digs)
  (if (endp digs)
      nil
      (let ((c (count (first digs) digs)))
        (cons c (group-lengths (nthcdr c digs))))))

(defun meets-criteria-a (digs)
  (and (apply #'<= digs)
       (some (lambda (group-len)
               (>= group-len 2))
             (group-lengths digs))))

(defun meets-criteria-b (digs)
  (and (apply #'<= digs)
       (some (lambda (group-len)
               (= group-len 2))
             (group-lengths digs))))

(defun solve ()
  (loop for i from 172851 upto 675869
     for digs = (digits i nil)
     count (meets-criteria-a digs) into sol-a
     count (meets-criteria-b digs) into sol-b
     finally (return (values sol-a sol-b))))
