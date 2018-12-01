(defun read-input-file ()
  (with-open-file (in (open "input.lisp"))
    (read in)))

(defparameter input-data (read-input-file))
