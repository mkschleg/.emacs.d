

(defvar org-daily--file "daily.org")


(defun org-daily-insert-today ()
  (interactive)
  (let ((bfr (find-buffer-visiting (concat org-directory org-daily--file)))
	(date (calendar-current-date)))
    (if bfr
	(switch-to-buffer bfr)
      (find-file-existing (concat org-directory org-daily--file)))
    (end-of-buffer)
    (search-backward-regexp "^* ")
    (if (= (nth 2 date) (string-to-number (org-entry-get (point) "ITEM")))
	(progn
	  (org-end-of-subtree)
	  (org-up-heading-safe))
      (org-insert-heading)
      (insert (nth 2 date))
      (org-insert-subheading)
      (insert (calendar-month-name (nth 0 date))))
    (if (string= (calendar-month-name (nth 0 date)) (org-entry-get (point) "ITEM"))
	(progn
	  (org-end-of-subtree)
	  (org-insert-heading)
	  (org-insert-time-stamp (current-time) nil t))
      (org-insert-heading)
      (insert (calendar-month-name (nth 0 date)))
      (org-insert-subheading)
      (org-insert-time-stamp (current-time) nil t))))



(root-leader
  "d" 'org-daily-insert-today)

(provide 'init-org-daily)

