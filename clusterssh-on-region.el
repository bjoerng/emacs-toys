(defun clusterssh-on-region ()
  "Sent region to clusterssh"
  (interactive)
  (apply 'start-process "clusterssh-process" "clusterssh-buffer" "clusterssh" (get-host-in-region-as-list)))

(defun get-host-in-region-as-list ()
  (let* ((region-string (buffer-substring-no-properties (region-beginning) (region-end))))
    (split-string region-string)))

(global-set-key (kbd "C-c c") 'clusterssh-on-region)

(defun clusterssh-on-rectangle ()
  (interactive)
    (let* ((region-strings  (extract-rectangle (region-beginning) (region-end))))
      (mapcar (lambda (string-val) (set-text-properties 0 (length string-val) nil string-val)) region-strings)
      (apply 'start-process "clusterssh-process" "clusterssh-buffer" "clusterssh"
	     (mapcar #'string-trim region-strings))))

(global-set-key (kbd "C-c r") 'clusterssh-on-rectangle)
