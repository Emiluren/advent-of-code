(ql:quickload "split-sequence")

(defun read-input-file ()
  (with-open-file (in (open "input"))
    (loop for line = (read-line in nil)
	   while line collect line)))

(defun split-on-spaces (line)
  (split-sequence:SPLIT-SEQUENCE #\Space line))

(defun split-lines-on-spaces (lines)
  (mapcar #'split-on-spaces lines))

(defun legal-passphrase (words)
  (or (null words)
	  (and (not (member (first words) (rest words) :test #'string=))
		   (legal-passphrase (rest words)))))

(defun count-legal-lines (lines)
  (length (remove-if-not #'legal-passphrase lines)))

(defun read-file-and-split-words ()
  (split-lines-on-spaces (read-input-file)))

(defun solve-part-1 ()
  (count-legal-lines (read-file-and-split-words)))

(defun sort-words (words)
  (mapcar (lambda (str) (sort str #'char-lessp)) words))

(defun solve-part-2 ()
  (count-legal-lines (mapcar #'sort-words (read-file-and-split-words))))
