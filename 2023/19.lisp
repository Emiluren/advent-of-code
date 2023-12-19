(ql:quickload "cl-ppcre")
(ql:quickload "trivia")
(ql:quickload "alexandria")

(defparameter *input* (uiop:read-file-string "19input"))

(defmacro if-let ((var test-form) then-form &optional else-form)
  `(let ((,var ,test-form))
     (if ,var ,then-form ,else-form)))

(defun parse-rule (rule-str)
  (ppcre:all-matches-as-strings "([xmas])|([<>])|([0-9]+)|(:)|(\\w+)" rule-str))

(defun parse-workflow (workflow-str)
  (ppcre:register-groups-bind ((#'intern workflow) rules) ("(\\w+){(.*)}" workflow-str)
    (cons workflow
          (mapcar #'parse-rule (ppcre:split "," rules)))))

(defun parse-part (part-str)
  (loop for val-str in (ppcre:split "," (subseq part-str 1 (1- (length part-str))))
        collect (cons (elt val-str 0) (parse-integer (subseq val-str 2 (length val-str))))))

(destructuring-bind (rules parts) (ppcre:split "\\n\\n" *input*)
  (defparameter *workflows* (mapcar #'parse-workflow (ppcre:split "\\n" rules)))
  (defparameter *parts* (mapcar #'parse-part (ppcre:split "\\n" parts))))

(defun part-val (part var)
  (alexandria:assoc-value part var))

(defun match-rule (part wf)
  (loop named rule-loop
        for rule in wf
        do (trivia:match rule
               ((list sym-str "<" num-str ":" action)
                (if (< (part-val part (elt sym-str 0)))
                    ))
             ((list )))))

(defun handle-wf (part)
  (loop named wf-loop
        for sym = '|in| then (match-rule part (alexandria:assoc-value *workflows* sym))
        do (case sym
             ('A (return-from wf-loop (loop for v in '(|x| |m| |a| |s|) sum (part-val part v))))
             ('R (return-from wf-loop 0)))))

(defun part1 ()
  (loop for part in *parts*
        sum (handle-wf part)))
