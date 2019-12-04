(defun digits (num digs)
  (if (< num 10)
      (cons num digs)
      (multiple-value-bind (quot rem) (floor num 10)
        (digits quot (cons rem digs)))))

(defun two-adjacent-p (digs)
  (when digs
    (let ((d1 (first digs)) (d2 (nth 1 digs)))
      (or (and d1 d2 (= d1 d2))
          (two-adjacent-p (rest digs))))))

(defun meets-criteria (digs)
  (and (apply #'<= digs)
       (two-adjacent-p digs)))

(defun solve-a ()
  (loop for i from 172851 upto 675869
     count (meets-criteria (digits i nil))))
