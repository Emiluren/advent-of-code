(defun read-packet (stream char)
  (declare (ignore char))
  (if (char= (peek-char t stream t nil t) #\])
      nil
      (let* ((first-obj (read stream t nil t))
             (next-char (read-char stream)))
        (cond ((char= next-char #\,)
               (cons first-obj (read-packet stream next-char)))
              ((char= next-char #\])
               (list first-obj))
              (t (error "Expected comma or closing bracket, got ~c" next-char))))))

(defun parse-packet (line)
  (let ((*readtable* (copy-readtable)))
     (set-macro-character #\[ 'read-packet)
     (set-macro-character #\] (get-macro-character #\)))
     (read-from-string line)))

(defun read-from-input-file (filename)
  (with-open-file (in filename :direction :input)
    (loop
       for line1 = (read-line in nil)
       for line2 = (read-line in nil)
       while (and line1 line2)
       do (read-line in nil)
       collect (list (parse-packet line1)
                     (parse-packet line2)))))

(defvar *input* (read-from-input-file "13input"))

(defun valid-packet-p (left right)
  ;;(format t "comparing ~a vs ~a~%" left right)
  (etypecase left
    (null (if (null right) 0 1))
    (integer (etypecase right
               (integer (- right left))
               (list (valid-packet-p (list left) right))))
    (list (etypecase right
            (null -1)
            (integer (valid-packet-p left (list right)))
            (list (let ((ret (valid-packet-p (first left) (first right))))
                    (cond ((> ret 0) 1)
                          ((= ret 0) (valid-packet-p (rest left) (rest right)))
                          (t -1))))))))

(defun part-1 ()
  (loop for (left right) in *input*
        for i upfrom 1
        ;;do (format t "i = ~a~%" i)
        when (when (>= (valid-packet-p left right) 0)
               ;;(format t "is valid~%")
               t)
          sum i
        ;;do (format t "~%")
        ))
