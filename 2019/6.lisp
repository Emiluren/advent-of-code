(load "6input.lisp")

(defparameter *parent-table* (make-hash-table))

(loop for (parent . child) in +input+
   do (setf (gethash child *parent-table*) parent))

(defun count-parents (node)
  (let ((p (gethash node *parent-table*)))
    (if p
        (1+ (count-parents p))
        0)))

(defun solve-a ()
  (loop for node being the hash-keys in *parent-table*
     sum (count-parents node)))


;; Below is just some code that makes a hash table that contains a list of
;; all children for every node. It was not used for the solution
(defparameter *children* (make-hash-table))

(loop for (parent . child) in +input+
   do (setf (gethash parent *children*)
            (cons child
                  (gethash parent *children*))))
