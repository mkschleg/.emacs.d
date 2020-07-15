


(require 'org)




(defun org-ICLR-insert-talk-link ()
  (interactive)
  (let ((talk-link (read-string "Enter Talk Link: ")))
    (org-insert-link "https:"
		     talk-link
		     "talk")))


(defun org-ICLR-insert-paper-links ()
  (interactive)
  (let ((spotlight-link (read-string "Enter Spotlight Link: "))
	(paper-link (read-string "Enter Paper Link: ")))
    (org-insert-link "https:"
		     paper-link
		     "paper")
    (insert-char ?\s 3)
    (org-insert-link "https:"
		     spotlight-link
		     "spotlight")))



