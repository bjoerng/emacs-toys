(defun goto-first-difference-of-two-lines ()
  (interactive)
  (forward-char
   (save-excursion
     (let* ((begin-first-string (progn
				  (move-beginning-of-line nil)
				  (point)))
	    (first-string (progn
			    (move-end-of-line nil)
			    (buffer-substring-no-properties
			     begin-first-string
			     (point))))
	    (begin-second-string (progn
				   (next-line)
				   (move-beginning-of-line nil)
				   (point)))
	    (second-string (progn
			     (move-end-of-line nil)
			     (buffer-substring-no-properties
			      begin-second-string
			      (point))))
	    (char-cons (mapcar* #'cons first-string second-string))
	    (char-cons-with-number (mapcar* #'cons char-cons (number-sequence
							      0
							      (cl-list-length char-cons))))
	    (forward-count (cl-dolist (elem char-cons-with-number)
			     (when (/= (caar elem) (cdar elem))
			       (cl-return  (cdr elem))))))
       forward-count))))

(global-set-key (kbd "C-c d") 'clusterssh-on-rectangle)
