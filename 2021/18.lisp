(defun read-left-bracket (stream char)
  (declare (ignore char))
  (let* ((first-obj (read stream t nil t))
         (next-char (peek-char t stream t nil t)))
    (if (char= next-char #\,)
          (read-char stream)
          (error "Expected comma, got ~c" next-char))
    (let* ((snd-obj (read stream t nil t))
           (next-char (peek-char t stream t nil t)))
      (if (char= next-char #\])
          (read-char stream)
          (error "Expected closing bracket ], got ~c" next-char))
      (cons first-obj snd-obj))))

(defun parse-snailfish (line)
  (let ((*readtable* (copy-readtable)))
     (set-macro-character #\[ 'read-left-bracket)
     (set-macro-character #\] (get-macro-character #\)))
     (read-from-string line)))

(defun read-from-input-file ()
  (with-open-file (in "18input" :direction :input)
    (loop
       for line = (read-line in nil)
       while line
       collect (parse-snailfish line))))

(defun add-deep-car (v add)
  (cond ((null add) v)
        ((atom v) (+ v add))
        (t (cons (add-deep-car (car v) add)
                 (cdr v)))))

(defun add-deep-cdr (v add)
  (cond ((null add) v)
        ((atom v) (+ v add))
        (t (cons (car v)
                 (add-deep-cdr (cdr v) add)))))

(defun explode-snailfish (snailfish depth)
  (if (atom snailfish)
      nil
      (let ((left-v (car snailfish))
            (right-v (cdr snailfish)))
        (if (and (> depth 4) (atom left-v) (atom right-v))
            (values 0 left-v right-v)
            (multiple-value-bind (new-left add-left add-right)
                (explode-snailfish left-v (1+ depth))
              (if new-left
                  (values (cons new-left (add-deep-car right-v add-right))
                          add-left
                          nil)
                  (multiple-value-bind (new-right add-left add-right)
                      (explode-snailfish right-v (1+ depth))
                    (if new-right
                        (values (cons (add-deep-cdr left-v add-left) new-right)
                                nil
                                add-right)
                        nil))))))))

(defmacro when-let ((var test-form) &rest body)
  `(let ((,var ,test-form))
     (when ,var
       ,@body)))

(defun split-snailfish (snailfish)
  (if (atom snailfish)
      (if (>= snailfish 10)
          (multiple-value-bind (q r) (floor snailfish 2)
            (cons q (+ q r))))
      (or (when-let (new-car (split-snailfish (car snailfish)))
            (cons new-car (cdr snailfish)))
          (when-let (new-cdr (split-snailfish (cdr snailfish)))
            (cons (car snailfish) new-cdr)))))

(defun reduce-snailfish (snailfish)
  (loop for new-fish = (or (explode-snailfish snailfish 1)
                           (split-snailfish snailfish))
        while new-fish
        do (setf snailfish new-fish)
        finally (return snailfish)))

(defun snailfish-magnitude (snailfish)
  (if (atom snailfish)
      snailfish
      (+ (* (snailfish-magnitude (car snailfish)) 3)
         (* (snailfish-magnitude (cdr snailfish)) 2))))

(defun snailfish-string (snailfish)
  (if (atom snailfish)
      (write-to-string snailfish)
      (format nil
              "[~a,~a]"
              (snailfish-string (car snailfish))
              (snailfish-string (cdr snailfish)))))

(defun part-1 ()
  (loop with last-fish = nil
        for snailfish in (read-from-input-file)
        do (setf last-fish (reduce-snailfish (if last-fish
                                                 (cons last-fish snailfish)
                                                 snailfish)))
        finally (return (snailfish-magnitude last-fish))))

(defun part-2 ()
  (loop with fishes = (read-from-input-file)
        for sn1 in fishes
        maximize (loop for sn2 in fishes
                       maximize (snailfish-magnitude (reduce-snailfish (cons sn1 sn2))))))
