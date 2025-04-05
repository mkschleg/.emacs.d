


(defun mattroot/org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))
(defun mattroot/org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(setq org-agenda-custom-commands
      '(("d" "Daily agenda and all TODOs"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo 'done)
						(org-agenda-skip-entry-if 'scheduled 'deadline)))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (tags "TODO=\"waiting\""
                ((org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo 'done)
						(org-agenda-skip-if nil '(scheduled))))
                 (org-agenda-overriding-header "Waiting:")))
	  (tags "PRIORITY=\"B\""
                ((org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo 'done)
						(org-agenda-skip-if nil '(scheduled))))
                 (org-agenda-overriding-header "Mid-priority unfinished tasks:")))
          (tags "PRIORITY=\"C\""
                ((org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo 'done)
						(org-agenda-skip-if nil '(scheduled))))
                 (org-agenda-overriding-header "Low-priority unfinished tasks:")))
          )
         ((org-agenda-compact-blocks t)))))


(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(p)" "|" "DONE(d)")
        (sequence "TODO-PROJ" "|" "DONE-PROJ")
        (sequence "TODO-LO" "|" "LEARNED")
        (sequence "TODO-EXP(e)" "RUNNING(r)" "TO-ANALYZE(o)" "|" "ANALYZED(a)")))

;; Set colors for todo states
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "red" :weight bold))
        ("TODO-PROJ" . (:foreground "red" :weight bold))
        ("TODO-LO" . (:foreground "red" :weight bold))
        ("TODO-EXP" . (:foreground "red" :weight bold))
        ("NEXT" . (:foreground "yellow" :weight bold))
        ("DONE" . org-done)
        ("DONE-PROJ" . org-done)
        ("LEARNED" . org-done)
        ("IN-PROGRESS" . (:foreground "yellow" :weight bold))
        ("WAITING" . (:foreground "yellow" :weight bold))
        ("RUNNING" . (:foreground "lightblue" :weight bold))
	("TO-ANALYZE" . (:foreground "orange" :weight bold))))

;; Automatic todo updating for trees
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0)
                  "DONE"
                (if (= n-done 0)
                    "TODO"
                  "IN-PROGRESS")))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(setq org-log-done 'time)

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
	      ("WAITING" ("WAITING" . t))
	      ("HOLD" ("WAITING") ("HOLD" . t))
	      (done ("WAITING") ("HOLD"))
	      ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
	      ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
	      ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))



(provide 'init-org-agenda)
