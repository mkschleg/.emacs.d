
;;;;;;;;;;
;; org-justify-fragment-overlay
;;   Used to justify the latex fragment preview. I use it to center equations, which feels nicer when looking at
;;   notes.
;; https://kitchingroup.cheme.cmu.edu/blog/2016/11/06/Justifying-LaTeX-preview-fragments-in-org-mode/
;;;;;;;;

;; specify the justification you want
(plist-put org-format-latex-options :justify 'center)

(defun scimax-org-latex-fragment-justify (justification)
  "Justify the latex fragment at point with JUSTIFICATION.
JUSTIFICATION is a symbol for 'left, 'center or 'right."
  (interactive
   (list (intern-soft
          (completing-read "Justification (left): " '(left center right)
                           nil t nil nil 'left))))
  (let* ((ov (ov-at))
	 (beg (ov-beg ov))
	 (end (ov-end ov))
	 (shift (- beg (line-beginning-position)))
	 (img (overlay-get ov 'display))
	 (img (and (and img (consp img) (eq (car img) 'image)
			(image-type-available-p (plist-get (cdr img) :type)))
		   img))
	 space-left offset)
    (when (and img
	       ;; This means the equation is at the start of the line
	       (= beg (line-beginning-position))
	       (or
		(string= "" (s-trim (buffer-substring end (line-end-position))))
		(eq 'latex-environment (car (org-element-context)))))
      (setq space-left (- (window-max-chars-per-line) (car (image-size img)))
	    offset (floor (cond
			   ((eq justification 'center)
			    (- (/ space-left 2) shift))
			   ((eq justification 'right)
			    (- space-left shift))
			   (t
			    0))))
      (when (>= offset 0)
	(overlay-put ov 'before-string (make-string offset ?\ ))))))

(defun scimax-org-latex-fragment-justify-advice (beg end image imagetype)
  "After advice function to justify fragments."
  (message "Justify Latex Fragment")
  (scimax-org-latex-fragment-justify (or (plist-get org-format-latex-options :justify) 'left)))


(defun scimax-toggle-latex-fragment-justification ()
  "Toggle if LaTeX fragment justification options can be used."
  (interactive)
  (if (not (get 'scimax-org-latex-fragment-justify-advice 'enabled))
      (progn
	(advice-add 'org--format-latex-make-overlay :after 'scimax-org-latex-fragment-justify-advice)
	(put 'scimax-org-latex-fragment-justify-advice 'enabled t)
	(message "Latex fragment justification enabled"))
    (advice-remove 'org--format-latex-make-overlay 'scimax-org-latex-fragment-justify-advice)
    (put 'scimax-org-latex-fragment-justify-advice 'enabled nil)
    (message "Latex fragment justification disabled")))

;; advise the function to a
;; (advice-add 'org--format-latex-make-overlay :after 'org-justify-fragment-overlay)
;; (advice-add 'org--format-latex-make-overlay :after 'org-latex-fragment-tooltip)


(provide 'init-org-equations)
