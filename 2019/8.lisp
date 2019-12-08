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

(defun print-image (image)
  (loop for y below (array-dimension image 0)
     do (loop for x below (array-dimension image 1)
           do (format t "~{~[ ~;0~; ~]" (aref image y x))
           finally (terpri))))

(defun solve-b ()
  (loop with image = (make-array (list (array-dimension *input-array* 1)
                                       (array-dimension *input-array* 2))
                                 :initial-element 2)
     for layer-i below (array-dimension *input-array* 0)
     do (loop for y below (array-dimension *input-array* 1)
           do (loop for x below (array-dimension *input-array* 2)
                 when (= (aref image y x) 2)
                 do (setf (aref image y x)
                          (aref *input-array* layer-i y x))))
     finally (print-image image)))
