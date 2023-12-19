(ql:quickload "cl-ppcre")
(ql:quickload "trivia")
(ql:quickload "alexandria")

(defparameter *input* (uiop:read-file-string "19input"))

(defmacro if-let ((var test-form) then-form &optional else-form)
  `(let ((,var ,test-form))
     (if ,var ,then-form ,else-form)))

(defun char-parser (retval)
  (lambda (stream char)
    (declare (ignore stream char))
    retval))

(defun parse-rule (rule-str)
  (let ((*readtable* (copy-readtable))
        (s rule-str)
        (start 0)
        token)
    (set-macro-character #\: (char-parser #\:))
    (set-macro-character #\< (char-parser '<))
    (set-macro-character #\> (char-parser '>))
    (loop do (multiple-value-bind (obj pos) (read-from-string s nil nil :start start)
               (setf start pos)
               (setf token obj))
          while token collect token)))

(defun parse-workflow (workflow-str)
  (ppcre:register-groups-bind ((#'read-from-string workflow) rules) ("(\\w+){(.*)}" workflow-str)
    (cons workflow
          (mapcar #'parse-rule (ppcre:split "," rules)))))

(defun parse-part (part-str)
  (loop for val-str in (ppcre:split "," (subseq part-str 1 (1- (length part-str))))
        collect (cons (read-from-string (subseq val-str 0 1)) (parse-integer (subseq val-str 2 (length val-str))))))

(destructuring-bind (rules parts) (ppcre:split "\\n\\n" *input*)
  (defparameter *workflows* (mapcar #'parse-workflow (ppcre:split "\\n" rules)))
  (defparameter *parts* (mapcar #'parse-part (ppcre:split "\\n" parts))))

(defun part-val (part var)
  (alexandria:assoc-value part var))

(defun match-rule (part wf)
  (loop named rule-loop
        for rule in wf
        do (trivia:match rule
             ((list sym '< num #\: action)
              (if (< (part-val part sym) num)
                  (return-from rule-loop action)))
             ((list sym '> num #\: action)
                (if (> (part-val part sym) num)
                    (return-from rule-loop action)))
             ((list sym)
              (return-from rule-loop sym)))))

(defun handle-wf (part)
  (loop named wf-loop
        for sym = 'in then (match-rule part (alexandria:assoc-value *workflows* sym))
        do (case sym
             (A (return-from wf-loop (loop for v in '(x m a s) sum (part-val part v))))
             (R (return-from wf-loop 0)))))

(defun part1 ()
  (loop for part in *parts*
        sum (handle-wf part)))
