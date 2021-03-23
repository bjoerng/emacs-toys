(defun bjoerng-sort-fields-in-region-per-line (delimiter)
  "Sorts all fields per line in region alphabetically. Fields are
determined by the delimiter."
  (interactive "MPlease provide delimiter: ")
  (let* ((lines (split-string (buffer-substring-no-properties
				(region-beginning) (region-end)) "\n"))
	 (new-lines (mapcar
		     (lambda (line-string)
		       (mapconcat #'identity
				  (sort (split-string line-string
						      delimiter)
					#'string<) delimiter))
		     lines) )
	 (new-string (mapconcat #'identity new-lines "\n")))
    (kill-region (region-beginning) (region-end))
    (insert new-string)))
