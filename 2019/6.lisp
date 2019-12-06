(load "6input.lisp")

(defparameter *parent-table* (make-hash-table))

(loop for (parent . child) in +input+
   do (setf (gethash child *parent-table*) parent))

(defun get-parents (node)
  (let ((p (gethash node *parent-table*)))
    (if p
        (cons p (get-parents p))
        nil)))

(defun solve-a ()
  (loop for node being the hash-keys in *parent-table*
     sum (length (get-parents node))))

(defun solve-b ()
  (let ((you-parents (get-parents 'you))
        (san-parents (get-parents 'san)))
    (loop for node in you-parents
       for i from 0
       do (let ((common-ancestor (position node san-parents)))
            (when common-ancestor
              (return (+ i common-ancestor)))))))


;; Below is just some code that makes a hash table that contains a list of
;; all children for every node. It was not used for the solution
(defparameter *children* (make-hash-table))

(loop for (parent . child) in +input+
   do (setf (gethash parent *children*)
            (cons child
                  (gethash parent *children*))))
