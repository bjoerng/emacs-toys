;;; clusterssh-on-region.el --- A collection of functions to run clusterssh or
;;; other shell commands on a region, a rectangle or every nth word in
;;; the lines in region.

;;; Commentary:
;; Known issues:
;; - Only one ececution of shell commands at a time,
;;   everything else leads to chaos and confusion.

;;; Code:

;; The default pack size for sconwiemlirphaat.
(defconst sconwiemlirphaat-packsize-default 30)
;(setq sconwiemlirphaat-packsize 30)

(defun get-every-nth-elem-in-list-with-first (n in-list)
  "Returns every Nth element in IN-LIST. The first element of
IN-LINE will be returned, the next N minus one will be
skipped. The next ohne will be returned."
  (mapcar #'cdr
	  (remove-if (lambda (cons-elem) (/= (% (car cons-elem) n) 0))
		     (mapcar* #'cons
			      (number-sequence 0 (- (length in-list) 1))
			      in-list))))

(defun get-every-nth-line-in-region-as-list (n)
  "Returns every Nth line in region, starting with first. So the
first line in region will be returned, the next n mins one will
be skipped. The next line will be returned and so on."
  (let* ((m (if n n 1)))
    (get-every-nth-elem-in-list-with-first
     m
     (split-string (buffer-substring-no-properties
		    (region-beginning)
		    (region-end))
		   "\n"))))

(defun get-mth-word-in-every-nth-line (word-m line-n)
  "Returns the WORD-Mth word in every LINE-Nth line. "
  (let* ((line-list (mapcar #'split-string
			    (get-every-nth-line-in-region-as-list line-n)))
	 (selected-words (mapcar (lambda (word-line)
				   (nth word-m word-line))
				 line-list)))
    selected-words))

(defun sconwiemlirphaat-sentinel-helper (process msg)
  "This function is called to start sconwiemlirphaat and is
called repeatedly after the shell command to execute is finished,
with the current pack. It starst the next ececution of
SCONWIEMLIRPHAAT-SHELL-COMMANED with the next pack of "
  (let* ((next-pack (seq-take sconwiemlirphaat-remaining-hosts
			      sconwiemlirphaat-packsize)))
    (if msg (print msg)
      (print (concat "Starting " sconwiemlirphaat-shell-command)))
    (setq sconwiemlirphaat-remaining-hosts
	  (seq-drop
	   sconwiemlirphaat-remaining-hosts
	   sconwiemlirphaat-packsize))
    (when next-pack
      (setq next-hosts-string (mapconcat #'identity next-pack " "))
      (print (concat "Next hosts: " next-hosts-string))
      (set-process-sentinel (apply #'start-process
				   sconwiemlirphaat-shell-command
				   (concat sconwiemlirphaat-shell-command "-Buffer")
				   sconwiemlirphaat-shell-command
				   next-pack)
		'sconwiemlirphaat-sentinel-helper))))

(defun sconwiemlirphaat (line-m word-n packsize command-to-exec)
  "Shell Command On Nth Word In Every Mth Line In Region P Hosts At A Time.
Runs COMMAND-TO-EXEC for the WORD-Nth word in every LINE-Mth"
  (interactive (list (read-number "What line divisor? " 1)
		     (read-number "Which word in line? " 1)
		     (read-number "How many hosts at once?"
				  sconwiemlirphaat-packsize-default)
		     (read-shell-command
		      "Please enter command to execute:")))
  (progn
    (makunbound 'sconwiemlirphaat-packsize)
    (defvar sconwiemlirphaat-packsize packsize)
    (makunbound 'sconwiemlirphaat-shell-command)d
    (defvar sconwiemlirphaat-shell-command command-to-exec)
    (makunbound 'sconwiemlirphaat-remaining-hosts)
    (defvar sconwiemlirphaat-remaining-hosts
      (remove nil
	      (get-mth-word-in-every-nth-line
	       (- word-n 1 ) line-m)))
    (sconwiemlirphaat-sentinel-helper nil nil)))

(defun get-words-in-region-as-list ()
  "Returns every word in region as a list."
  (let* ((region-string (buffer-substring-no-properties
			 (region-beginning)
			 (region-end))))
    (split-string region-string)))


(defun sconwiemlirphaat-reset ()
  "Resets the current sconwiemlirphaat execution."
  (interactive)
  (makunbound 'sconwiemlirphaat-remaining-hosts)
  (makunbound 'sconwiemlirphaat-pack-size)
  (makunbound 'sconwiemlirphaat-shell-command)
  (makunbound 'sconwiemlirphaat-pack-size))

(defun clusterssh-on-region (word-in-line)
  "Sent region to clusterssh. If WORD-IN-LINE is given, the
WORD-IN-LINEth word of each line will be extractet. "
  (interactive "P")
  (progn
    (makunbound 'sconwiemlirphaat-remaining-hosts)
    (if word-in-line
	(defvar sconwiemlirphaat-remaining-hosts
	  (get-mth-word-in-every-nth-line (1- word-in-line) 1))
      (defvar sconwiemlirphaat-remaining-hosts
	(get-words-in-region-as-list)))
    (makunbound 'sconwiemlirphaat-shell-command)
    (defvar sconwiemlirphaat-packsize sconwiemlirphaat-packsize-default)
    (defvar sconwiemlirphaat-shell-command "clusterssh")
    (sconwiemlirphaat-sentinel-helper nil nil)))

(global-set-key (kbd "C-c c") 'clusterssh-on-region)

(defun clusterssh-on-rectangle ()
  "Send rectangle to cluserssh."
  (interactive)
    (let* ((rectangle-strings  (extract-rectangle (region-beginning) (region-end))))
      (mapcar (lambda (string-val) (set-text-properties 0
							(length string-val)
							nil string-val))
	      rectangle-strings)
      (makunbound 'sconwiemlirphaat-remaining-hosts)
      (defvar sconwiemlirphaat-remaining-hosts rectangle-strings)
      (defvar sconwiemlirphaat-packsize sconwiemlirphaat-packsize-default)
      (sconwiemlirphaat-sentinel-helper nil nil)))
      
(global-set-key (kbd "C-c r") 'clusterssh-on-rectangle)

;;; clusterssh-on-region.el ends here
