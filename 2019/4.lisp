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

(defun meets-criteria-a (digs)
  (and (apply #'<= digs)
       (two-adjacent-p digs)))

(defun group-digits (digs current-group groups)
  (if (not digs)
      (if current-group
          (cons current-group groups)
          groups)
      (if (and current-group (= (first digs) (first current-group)))
          (group-digits (rest digs)
                        (cons (first digs) current-group)
                        groups)
          (group-digits (rest digs)
                        (list (first digs))
                        (if current-group
                            (cons current-group groups)
                            groups)))))

(defun meets-criteria-b (digs)
  (and (apply #'<= digs)
       (some (lambda (group)
               (= (length group) 2))
             (group-digits digs nil nil))))

(defun solve ()
  (loop for i from 172851 upto 675869
     for digs = (digits i nil)
     count (meets-criteria-a digs) into sol-a
     count (meets-criteria-b digs) into sol-b
     finally (return (values sol-a sol-b))))
