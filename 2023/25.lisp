(defparameter *input*
  (loop for line in (uiop:read-file-lines "25input")
        collect (loop with symbol-string = (remove #\: line)
                      and pos = 0
                      with sym
                      do (setf (values sym pos) (read-from-string symbol-string nil nil :start pos))
                      collect sym
                      until (>= pos (length symbol-string)))))

(defparameter *connections*
  (loop for line in *input*
        nconc (loop for other-component in (rest line)
                    collect (cons (first line) other-component))))

(defparameter *components* ())
(loop for (a . b) in *connections*
      do (setf *components* (adjoin a (adjoin b *components*))))

(defun random-elt (l)
  (elt l (random (length l))))

;; Ugly hack with global variable
(defparameter *vert-hashes* (make-hash-table))

;; This would probably be faster with a disjoint sets datastructure
(defun contract-random-edge (edges verts)
  (let* ((e (random-elt edges))
         (v1 (car e))
         (v2 (cdr e)))
    (setf (gethash v1 *vert-hashes*) (union (gethash v1 *vert-hashes*)
                                            (gethash v2 *vert-hashes*)))
    (values (remove-if (lambda (e2) (eq (car e2) (cdr e2)))
                       (subst v1 v2 (remove e edges)))
            (remove v2 verts))))

(defun kargers ()
  (loop for v in *components*
        do (setf (gethash v *vert-hashes*) (list v)))
  (loop with edges = *connections*
        and verts = *components*
        while (> (length verts) 2)
        do (setf (values edges verts) (contract-random-edge edges verts))
        finally (return (values edges verts))))

;; Took 519 iterations lol
(defun part1 ()
  (loop with edges and verts
        for i from 1
        do (format t "Iteration ~a: " i)
           (setf *vert-hashes* (make-hash-table))
           (setf (values edges verts) (kargers))
           (format t "edge count = ~a~%" (length edges))
        while (> (length edges) 3)
        finally (return edges)))

(defun solve-part-1 ()
  (let* ((e (first (part1)))
         (v1 (car e))
         (v2 (cdr e)))
    (* (length (gethash v1 *vert-hashes*))
       (length (gethash v2 *vert-hashes*)))))
