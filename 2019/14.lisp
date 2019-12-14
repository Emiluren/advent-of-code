(defparameter *input-list*
  (with-open-file (stream "14input.lisp")
    (read stream)))

(defparameter *input-table*
  (loop with table = (make-hash-table)
     for (inputs . (output-amount . output)) in *input-list*
     do (setf (gethash output table) (cons output-amount inputs))
     finally (return table)))

(defun required-ore-for (amount chemical leftover-ingredients)
  (if (eq chemical 'ore)
      amount
      (destructuring-bind (recipe-amount . ingredients) (gethash chemical *input-table*)
        (let ((leftover-amount (gethash chemical leftover-ingredients 0)))
          (setf (gethash chemical leftover-ingredients) (max (- leftover-amount amount) 0)
                amount (max (- amount leftover-amount) 0))
          (multiple-value-bind (reaction-amount new-leftovers) (ceiling amount recipe-amount)
            (setf (gethash chemical leftover-ingredients)
                  (+ (gethash chemical leftover-ingredients 0)
                     (abs new-leftovers)))
            (loop for (input-amount . input) in ingredients
               sum (required-ore-for (* reaction-amount input-amount)
                                     input
                                     leftover-ingredients)))))))
