(defparameter *input* (with-open-file (in "input")
                        (read in)))

(defun mark-rect (arr k x y w h)
  (loop for i from 0 below w
        do (loop for j from 0 below h
                 do (setf (aref arr (+ x i) (+ y j))
                          (if (eql (aref arr (+ x i) (+ y j)) 0)
                              k
                              -1)))))

(defparameter *arr* (make-array '(1000 1000)))

(loop for line in *input*
      for k from 1
      do (apply #'mark-rect *arr* k line))

;; Part 1
(loop for x from 0 to 999
      sum (loop for y from 0 to 999
                count (> (aref *arr* x y) 1)))

;; Part 2
(defun all-positive (arr x y w h)
  (loop for i from 0 below w
        do (loop for j below h
                 do (if (< (aref arr (+ x i) (+ y j)) 0)
                        (return-from all-positive))))
  t)

(loop for line in *input*
      for i from 1
      until (apply #'all-positive *arr* line)
      finally (return i))

(defun sub-array (x y w h)
  (loop for i from 0 below w
        collect (loop for j from 0 below h
                      collect (aref *arr* (+ x i) (+ y j)))))
