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

(defun valid-packet-p (first-packet second-packet)
  (format t "comparing ~a vs ~a~%" first-packet second-packet)
  (etypecase first-packet
    (null t)
    (integer (etypecase second-packet
               (integer (if (< first-packet second-packet)
                            (values t t)
                            (= first-packet second-packet)))
               (list (valid-packet-p (list first-packet) second-packet))))
    (list (etypecase second-packet
            (integer (valid-packet-p first-packet (list second-packet)))
            (list (and (car second-packet)
                       (multiple-value-bind (first-valid smaller-first)
                           (valid-packet-p (first first-packet) (first second-packet))
                         (and first-valid
                              (or smaller-first
                                  (valid-packet-p (rest first-packet) (rest second-packet)))))))))))

(defun part-1 ()
  (loop for (first-packet second-packet) in *input*
        for i upfrom 1
        do (format t "i = ~a~%" i)
        when (valid-packet-p first-packet second-packet)
          sum i
        do (format t "~%")))
