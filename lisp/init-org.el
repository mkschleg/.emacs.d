
(use-package org
  ;; :ensure org-contrib
  :functions
  org-renumber-environment
  mattroot/org-skip-subtree-if-habit
  mattroot/org-skip-subtree-if-priority
  mattroot/org-export-output-file-name-modified
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :hook ((org-mode . (lambda () (visual-line-mode 1)))
         (org-mode . visual-fill-column-mode)
         (org-mode . (lambda () (setq visual-fill-column-center-text t)))
         (org-mode . (lambda () (setq fill-column 150)))
         (org-mode . org-indent-mode))
  :custom
  (org-directory "~/org/")
  (org-default-notes-file (concat org-directory "/refile.org"))
  (org-highlight-latex-and-related '(latex script entities))
  (org-agenda-files (list "~/org/"
                          "~/org/plan/"
                          "~/org/projects/"
                          "~/Documents/Research/thesis/thesis/thesis.org"
                          "~/Documents/Research/thesis/thesis/candidacy.org"))
  (org-startup-folded nil)
  
  (org-agenda-tag-filter-preset (quote
                                 ("-ignore")))

  (org-tag-alist '((:startgrouptag)
                   ("persp")
                   (:grouptags)
                   ("vision")
                   ("goal")
                   ("question")
                   ("scope")
                   (:endgrouptag)
                   (:startgrouptag)
                   ("action")
                   (:grouptags)
                   ("message")
                   ("review")
                   ("code")
                   ("read")
                   ("write")
                   ("analyze")
                   (:endgrouptag)
                   (:startgrouptag)
                   ("wait")
                   (:grouptags)
                   ("experiment")
                   ("colab")
                   ("admin")
                   (:endgrouptag)
                   (:startgrouptag)
                   ("note")
                   (:grouptags)
                   ("journal")
                   ("conclusion")
                   ("observation")
                   ("data")
                   (:endgrouptag)))

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
  (org-latex-pdf-process (list "latexmk --shell-escape -pdf -bibtex %f -output-directory=%o"))

  ;; Larger equations
  (org-latex-prefer-user-labels t)
  (org-latex-logfiles-extensions (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl")))
  (mattroot/org-pub-dir "/Users/Matt/org/org-export-files")
  (org-latex-packages-alist '(("" "natbib" nil)))

  
  (org-default-priority ?D)
  (org-lowest-priority ?D)

  (org-archive-location "::* Archive")

  (org-refile-targets '((nil :maxlevel . 1)
                       (org-agenda-files :maxlevel . 1)))
  
  (org-src-tab-acts-natively nil)

  ;; Org habit
  (org-habit-scheduled-past-days nil)
  (calendar-week-start-day 1)
  (indent-tabs-mode nil)
  
  :config

  (require 'org-habit)
  (require 'org-habit-plus)

  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

  (setq org-capture-templates
        '(("t" "Create generic task" entry
           (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n %u\n %i")
          ("i" "Create a inline Task" entry
           (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n %i\n %a")
          ("p" "Paper" entry
           (file+headline org-default-notes-file "Read List")
           "* TODO %^{TITLE}\n %i %u\n %i %^{LINK}\n")
          ("w" "Website" entry
           (file+headline org-default-notes-file "Read List")
           "* TODO %^{Topic}\n %i %u\n %i %^{LINK}\n")
          ("v" "Video" entry
           (file+headline org-default-notes-file "Watch List")
           "* TODO %^{Topic}\n %i %u\n %i %^{LINK}\n")))

  ;; Export details
  (add-to-list 'org-latex-packages-alist '("" "fullpage" nil))

  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)
  
  (setq org-support-shift-select 'always)

  (setcdr (assoc "\\.pdf\\'" org-file-apps) 'pdf-tools)

  (require 'init-org-macros)

  
  (setq org-export-global-macros mattroot/org-macros)
  (global-unset-key (kbd "C-c ["))
  (global-set-key [remap org-set-tags-command] #'counsel-org-tag)


  (setq indent-tabs-mode nil)
  (setq org-edit-src-content-indentation 0)
  (setq org-src-preserve-indentation t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))

  (require 'init-org-agenda)
  (require 'init-org-equations)
  (require 'ox-publish)

  );; use-package org31

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
  (define-key org-journal-mode-map (kbd "C-x C-s") 'org-journal-save-entry-and-exit)
  (setq org-default-priority ?D)
  (setq org-lowest-priority ?D))


;;;;
;; Pretty bullets
;;;;

(use-package org-bullets
  :ensure t
  :hook (org-mode . (lambda () (org-bullets-mode 1))))


(use-package org-inlinetask)

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
  (bibtex-completion-display-formats
	'((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
	  (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
	  (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}")))
  (bibtex-completion-bibliography '("~/org/bib/full_library.bib"))
  (reftex-default-bibliography '("~/org/bib/full_library.bib"))
  (org-ref-default-bibliography '("~/org/bib/full_library.bib"))
  :init
  (with-eval-after-load 'ox
    (defun my/org-ref-process-buffer--html (backend)
      "Preprocess `org-ref' citations to HTML format.

Do this only if the export backend is `html' or a derivative of
that."
      ;; `ox-hugo' is derived indirectly from `ox-html'.
      ;; ox-hugo <- ox-blackfriday <- ox-md <- ox-html
      (when (org-export-derived-backend-p backend 'html)
        (org-ref-process-buffer 'html)))
    (add-to-list 'org-export-before-parsing-hook #'my/org-ref-process-buffer--html))
  :config
  
  (require 'org-ref-ivy)

  (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
        org-ref-insert-cite-function 'org-ref-cite-insert-ivy
        org-ref-insert-label-function 'org-ref-insert-label-link
        org-ref-insert-ref-function 'org-ref-insert-ref-link
        org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body)))
  (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link))


;; (use-package org-fragtog
;;   :ensure t
;;   :after org
;;   :config
;;   (add-hook 'org-mode-hook #'org-fragtog-mode))

;;; requires pdf2svg
(use-package org-inline-pdf
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook #'org-inline-pdf-mode))


(require 'init-org-export)
(require 'init-org-babel)

(provide 'init-org)
