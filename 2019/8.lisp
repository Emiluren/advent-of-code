(defparameter *input-lists*
  (with-open-file (stream "8input.lisp")
    (read stream)))

(defparameter *zeroes-per-layer*
  (mapcar (lambda (layer)
            (apply #'+ (mapcar (lambda (row)
                                 (count 0 row))
                               layer)))
          *input-lists*))

(defun count-in-layer (item layer)
  (apply #'+ (mapcar (lambda (row)
                       (count item row))
                     layer)))

(defun solve-a ()
  (let* ((least-zeroes (reduce (lambda (a b) (min a b))
                               *zeroes-per-layer*))
         (least-zero-layer (position least-zeroes *zeroes-per-layer*)))
    (* (count-in-layer 1 (nth least-zero-layer *input-lists*))
       (count-in-layer 2 (nth least-zero-layer *input-lists*)))))

(defparameter *input-array*
  (make-array (list (length *input-lists*)
                    (length (car *input-lists*))
                    (length (caar *input-lists*)))
              :initial-contents *input-lists*))
