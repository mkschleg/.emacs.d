
(use-package org
  :functions
  org-renumber-environment
  mattroot/org-skip-subtree-if-habit
  mattroot/org-skip-subtree-if-priority
  mattroot/org-export-output-file-name-modified
  :bind (("C-c a" . org-agenda))
  :hook ((org-mode . (lambda () (visual-line-mode 1))))
  :custom
  (org-directory "~/org/")
  (org-default-notes-file (concat org-directory "/refile.org"))
  (org-highlight-latex-and-related '(latex script entities))
  (org-agenda-files (list "~/org/"
			  "~/org/recur/"
			  "~/Documents/Research/notes/projects/"
			  "~/Documents/Research/notes/topics/"
			  "~/Documents/Research/notes/courses/"))
  (org-agenda-tag-filter-preset (quote
                                 ("-ignore")))
  (org-treat-S-cursor-todo-selection-as-state-change nil)


  (org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                          (vm-imap . vm-visit-imap-folder-other-frame)
                          (gnus . org-gnus-no-new-news)
                          (file . find-file)
                          (wl . wl-other-frame)))
  ;; Ability to use key shortcuts for selecting a state
  (org-use-fast-todo-selection t)

  ;; Don't calculate the statistics of a todo item recursively through the tree
  (org-hierarchical-todo-statistics nil)

  ;; Org to latex pdf process
  (org-latex-pdf-process (list "latexmk -pdf -bibtex %f -output-directory=%o"))

  ;; Larger equations
  (org-latex-prefer-user-labels t)
  (org-latex-logfiles-extensions (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl")))
  (mattroot/org-pub-dir "/Users/Matt/org/org-export-files")
  (org-latex-packages-alist '(("" "natbib" nil)))
  
  (org-default-priority ?C)
  (org-lowest-priority ?D)
  
  (org-src-tab-acts-natively nil)

  ;; Org habit
  (org-habit-scheduled-past-days nil)
  (calendar-week-start-day 1)
  (indent-tabs-mode nil)
  :config

  (require 'org-habit-plus)

  ;; Export details
  (add-to-list 'org-latex-packages-alist '("" "fullpage" nil))

  ;; (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  ;; (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)
  (setq org-support-shift-select 'always)

  
  ;; (defun mattroot/org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
  ;;   (unless mattroot/org-pub-dir
  ;;     (setq mattroot/org-pub-dir "exported-org-files")
  ;;     (unless (file-directory-p mattroot/org-pub-dir)
  ;;       (make-directory mattroot/org-pub-dir)))
  ;;   (apply orig-fun extension subtreep mattroot/org-pub-dir nil))
  ;; (advice-add 'org-export-output-file-name :around #'mattroot/org-export-output-file-name-modified)

  (setcdr (assoc "\\.pdf\\'" org-file-apps) 'pdf-tools)

  (require 'init-org-macros)

  (setq org-export-global-macros mattroot/org-macros)

  (global-unset-key (kbd "C-c ["))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (jupyter . t)))

  (setq indent-tabs-mode nil)
  (setq org-edit-src-content-indentation 0)
  (setq org-src-preserve-indentation t)

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
						  (org-agenda-skip-if nil '(scheduled))))
                   (org-agenda-overriding-header "High-priority unfinished tasks:")))
	    (tags "REXP"
		  ((org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo 'done)
						  (org-agenda-skip-if nil '(scheduled))))
                   (org-agenda-overriding-header "Running Experiments:")))
	    (tags "EXP"
                  ((org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo 'done)
						  (org-agenda-skip-if nil '(scheduled))))
                   (org-agenda-overriding-header "Experiments:")))
            (agenda "" ((org-agenda-ndays 1)))
	    (tags "PRIORITY=\"B\""
                  ((org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo 'done)
	    					  (org-agenda-skip-if nil '(scheduled))))
                   (org-agenda-overriding-header "Mid-priority unfinished tasks:")))
            (alltodo ""
                     ((org-agenda-skip-function '(or (mattroot/org-skip-subtree-if-habit)
                                                     (mattroot/org-skip-subtree-if-priority ?A)
						     (mattroot/org-skip-subtree-if-priority ?B)
                                                     (org-agenda-skip-if nil '(scheduled deadline))))
                      (org-agenda-overriding-header "ALL normal priority tasks:"))))
           ((org-agenda-compact-blocks t)))))


  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(p)" "|" "DONE(d)")
          (sequence "WAITING(w)" "RUNNING(r)" "DOWNLOAD(o)" "|" "CANCELLED(c)")))

  ;; Set colors for todo states
  (setq org-todo-keyword-faces
	'(("TODO" . (:foreground "red" :weight bold))
          ("NEXT" . (:foreground "yellow" :weight bold))
          ("DONE" . org-done)
          ("IN-PROGRESS" . (:foreground "yellow" :weight bold))
          ("WAITING" . (:foreground "yellow" :weight bold))
          ("RUNNING" . (:foreground "lightblue" :weight bold))
	  ("DOWNLOAD" . (:foreground "orange" :weight bold))))

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

  (setq org-todo-state-tags-triggers
	(quote (("CANCELLED" ("CANCELLED" . t))
		("WAITING" ("WAITING" . t))
		("HOLD" ("WAITING") ("HOLD" . t))
		(done ("WAITING") ("HOLD"))
		("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
		("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
		("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))


  (require 'ox-publish)
  ;; (setq org-publish-project-alist
  ;;     '(
  ;;       ("braindump"
  ;;        :base-directory)
  ;;     ))

  
  ) ;; use-package org

(root-leader
  "d" '(:ignore t :which-key "[d]aily Journal"))


(use-package org-journal
  :ensure t
  :bind (("M-m d n" . 'org-journal-new-entry))
  :custom
  (org-journal-dir "~/org/journal")
  (org-journal-find-file 'find-file)
  :config
  (defun org-journal-save-entry-and-exit()
    "Simple convenience function.
  Saves the buffer of the current day's entry and kills the window
  Similar to org-capture like behavior"
    (interactive)
    (save-buffer)
    (kill-buffer-and-window))
  (define-key org-journal-mode-map (kbd "C-x C-s") 'org-journal-save-entry-and-exit))


;;;;
;; Pretty bullets
;;;;

(use-package org-bullets
  :ensure t
  :hook (org-mode . (lambda () (org-bullets-mode 1))))


;;;;
;; zotxt
;;;;

(use-package zotxt
  :ensure t
  :hook (org-mode . (lambda () (org-zotxt-mode 1)))
  :bind (("C-c \" o" . mattroot/org-zotxt-get-org-file-at-point))
  :functions
  mattroot/org-zotxt-org-file
  mattroot/org-zotxt-get-org-file-at-point
  :config
  ;; Change citation format to be less cumbersome in files.
  ;; You'll need to install matt-short into your style manager first.
  (setq zotxt-default-bibliography-style "matt-short")

  (defun mattroot/org-zotxt-org-file (item)
    ;; (message item)
    (downcase (s-replace-all '((" " . "_") (".pdf" . ".org"))
			     (nth 0 (last (split-string item "/"))))))

  (defun mattroot/org-zotxt-get-org-file-at-point (&optional arg)
    "Opens with `org-open-file', see for more information about ARG."
    (interactive "P")
    (lexical-let ((item-id (org-zotxt-extract-link-id-at-point))
                  (arg arg))
      (deferred:$
	(request-deferred
	 (format "%s/items" zotxt-url-base)
	 :params `(("key" . ,item-id) ("format" . "paths"))
	 :parser 'json-read)
	(deferred:nextc it
          (lambda (response)
            (let ((paths (cdr (assq 'paths (elt (request-response-data response) 0)))))
              (kill-new (mattroot/org-zotxt-org-file (org-zotxt-choose-path paths))))))
	(if zotxt--debug-sync (deferred:sync! it)))))

  )


(use-package org-ref
  :ensure t
  :custom
  (reftex-default-bibliography '("~/Google Drive/bib/full_library.bib"))
  (org-ref-default-bibliography '("~/Google Drive/bib/full_library.bib"))
  :config
  (org-ref-ivy-cite-completion))

(use-package ox-hugo
  :ensure t
  :after ox)


(provide 'init-org)
