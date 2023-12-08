(require "asdf")

(ql:quickload :alexandria)

(defparameter *input* (uiop:read-file-lines "8input"))

(setf *print-circle* t)
(defparameter *path*
  (loop for c across (first *input*)
        collect (intern (string c))))
(setf (cdr (last *path*)) *path*)

(defparameter *network*
  (loop for line in (nthcdr 2 *input*)
        for node = (subseq line 0 3)
        for left-node = (subseq line 7 10)
        for right-node = (subseq line 12 15)
        collect `(,(intern node) . (,(intern left-node) . ,(intern right-node)))))

(defun collect-ends-with (c)
  (loop
    for (n) in *network*
    if (char= (elt (string n) 2) c)
      collect n))

(defparameter *start-nodes* (collect-ends-with #\A))
(defparameter *end-nodes* (collect-ends-with #\Z))

(defun get-next (node dir)
  (destructuring-bind (left . right) (alexandria:assoc-value *network* node)
      (if (eq dir 'L)
          left
          right)))

(defun steps-to-reach (start)
  (loop
    for node = start then (get-next node c)
    for c in *path*
    for i from 0
    until (member node *end-nodes*)
    ;collect node into ns
    finally (return i)))

(defun part1 ()
  (steps-to-reach 'AAA))

(defun part2 ()
  (apply #'lcm (mapcar #'steps-to-reach *start-nodes*)))







;; Some experimentation for part 2
(defun indices-reached (start)
  (loop
    for node = start then (get-next node c)
    for c in *path*
    for i from 0 upto 100000
    if (member node *end-nodes*)
      collect i))

(defun diff-list (start)
  (let ((l (indices-reached start)))
    (mapcar #'- (rest l) (butlast l))))

;; Patterns repeat with intervals, they return to the start every time
; (13939 17621 19199 15517 12361 20777)
