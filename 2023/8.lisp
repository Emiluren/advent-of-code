(require "asdf")

(ql:quickload :alexandria)

(defparameter *input* (uiop:read-file-lines "8input"))
(defparameter *path* (first *input*))

(defparameter *network*
  (loop for line in (nthcdr 2 *input*)
        for node = (subseq line 0 3)
        for left-node = (subseq line 7 10)
        for right-node = (subseq line 12 15)
        collect `(,(intern node) . (,(intern left-node) . ,(intern right-node)))))

(defun part1 ()
  (loop
    with node = 'AAA
    for i from 0
    for c = (elt *path* (mod i (length *path*)))
    for (left . right) = (alexandria:assoc-value *network* node)
    do (setf node (if (char= c #\L)
                      left
                      right))
    while (not (eq node 'ZZZ))
    finally (return (1+ i))))
