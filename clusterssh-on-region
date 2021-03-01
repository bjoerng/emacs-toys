(defun clusterssh-on-region ()
  "Sent region to clusterssh"
  (interactive)
  (let ((proc (apply 'start-process "clusterssh-process" "clusterssh-buffer" "clusterssh" (get-host-in-region-as-list))))))

(defun get-host-in-region-as-list ()
  (let* ((region-string (buffer-substring-no-properties (region-beginning) (region-end))))
    (split-string region-string)))

(global-set-key (kbd "C-c c") 'clusterssh-on-region)
