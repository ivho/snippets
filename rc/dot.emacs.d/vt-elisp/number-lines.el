(defun number-lines (start every step)
  "Number lines in region, starting at number START. Every EVERY line
is numbered. The line number is increased by STEP every line."
  (interactive 
   (if current-prefix-arg
       (list (string-to-int (read-string "Start line: " "1"))
	     (string-to-int (read-string "Number every: " "1"))
	     (string-to-int (read-string "Line count step: " "1")))
     '(1 1 1)))
  (save-excursion
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (goto-char (region-beginning))
      (let ((count 1))
	(while (/= (point) (point-max))
	  (beginning-of-line)
	  (if (/= 0 (setq count (1- count)))
	      (insert-string "        ")
	    (insert-string (format "%6d  " start))
	    (setq count every))
	  (setq start (+ start step))
	  (forward-line))))))
