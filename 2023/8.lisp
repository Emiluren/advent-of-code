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

(defun steps-to-reach (start ends)
  (loop
    with node = start
    for i from 0
    for c = (elt *path* (mod i (length *path*)))
    for (left . right) = (alexandria:assoc-value *network* node)
    do (setf node (if (char= c #\L)
                      left
                      right))
    while (not (member node ends))
    finally (return (1+ i))))

(defun part1 ()
  (steps-to-reach 'AAA '(ZZZ)))

(defun collect-ends-with (c)
  (loop
    for (n) in *network*
    if (char= (elt (string n) 2) c)
      collect n))

(defparameter *start-nodes* (collect-ends-with #\A))
(defparameter *end-nodes* (collect-ends-with #\Z))

(defun part2 ()
  (loop
    with nodes = *start-nodes*
    for i from 0
    for c = (elt *path* (mod i (length *path*)))
    do (setf nodes
             (loop
               for n in nodes
               for (left . right) = (alexandria:assoc-value *network* n)
               collect (if (char= c #\L)
                           left
                           right)))
    while (some (lambda (n) (not (member n *end-nodes*))) nodes)
    finally (return (1+ i))))
