






(use-package org
  :functions
  org-renumber-environment
  :init
  (global-unset-key (kbd "C-c ["))
  :custom
  (org-directory "~/org/")
  (org-default-notes-file (concat org-directory "/refile.org"))
  (org-highlight-latex-and-related '(latex script entities))
  (org-agenda-files (list "~/org/"
			  "~/Documents/Research/notes/projects/"))
  (org-agenda-tag-filter-preset (quote
                                 ("-ignore")))
  (org-treat-S-cursor-todo-selection-as-state-change nil)
  ;; Ability to use key shortcuts for selecting a state
  (org-use-fast-todo-selection t)
  ;; Don't calculate the statistics of a todo item recursively through the tree
  (org-hierarchical-todo-statistics nil)
  ;; Org to latex pdf process
  (org-latex-pdf-process (list "latexmk -pdf -bibtex %f"))
  ;; Larger equations
  (org-latex-prefer-user-labels t)
  (org-latex-logfiles-extensions (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl")))
  :config
  
  (defun org-renumber-environment (orig-func &rest args)
  (let ((results '()) 
        (counter -1)
        (numberp))

    (setq results (loop for (begin .  env) in 
                        (org-element-map (org-element-parse-buffer) 'latex-environment
                          (lambda (env)
                            (cons
                             (org-element-property :begin env)
                             (org-element-property :value env))))
                        collect
                        (cond
                         ((and (string-match "\\\\begin{equation}" env)
                               (not (string-match "\\\\tag{" env)))
                          (incf counter)
                          (cons begin counter))
                         ((string-match "\\\\begin{align}" env)
                          (prog2
                              (incf counter)
                              (cons begin counter)                          
                            (with-temp-buffer
                              (insert env)
                              (goto-char (point-min))
                              ;; \\ is used for a new line. Each one leads to a number
                              (incf counter (count-matches "\\\\$"))
                              ;; unless there are nonumbers.
                              (goto-char (point-min))
                              (decf counter (count-matches "\\nonumber")))))
                         (t
                          (cons begin nil)))))

    (when (setq numberp (cdr (assoc (point) results)))
      (setf (car args)
            (concat
             (format "\\setcounter{equation}{%s}\n" numberp)
             (car args)))))
  
  (apply orig-func args))

  ;; (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.3))
  ;; (plist-put org-format-latex-options :scale 1.3)
  (advice-add 'org-create-formula-image :around #'org-renumber-environment)
  ;; (advice-add 'org-create-formula-image :around #'org-renumber-environment :scale ())

  )


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
  :config
  ;; Change citation format to be less cumbersome in files.
  ;; You'll need to install matt-short into your style manager first.
  (setq zotxt-default-bibliography-style "matt-short"))


(use-package org-ref
  :ensure t
  :custom
  (reftex-default-bibliography '("~/Google Drive/bib/full_library.bib"))
  (org-ref-default-bibliography '("~/Google Drive/bib/full_library.bib"))
  :config
  (org-ref-ivy-cite-completion))

;; (define-key org-mode-map
;;   (kbd "C-c \" \"") (lambda () (interactive)
;;                       (org-zotxt-insert-reference-link '(4))))

;;;;;;;
;; Org Mode Workflow
;;;;;;;


;; Set todo keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(p)" "|" "DONE(d)")
        (sequence "WAITING(w)" "RUNNING(r)" "|" "CANCELLED(c)")))

;; Set colors for todo states
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "red" :weight bold))
        ("NEXT" . (:foreground "yellow" :weight bold))
        ("DONE" . org-done)
        ("IN-PROGRESS" . (:foreground "yellow" :weight bold))
        ("WAITING" . (:foreground "yellow" :weight bold))
        ("RUNNING" . (:foreground "lightblue" :weight bold))))

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




;; (defun org-mode-reftex-setup ()
;;   (load-library "reftex")
;;   (and (buffer-file-name)
;;        (file-exists-p (buffer-file-name))
;;        (reftex-parse-all))
;;   (define-key org-mode-map (kbd "C-c )") 'reftex-citation))

;; (add-hook 'org-mode-hook 'org-mode-reftex-setup)


;; (setq org-latex-to-pdf-process '("latex %f && bibtex %f && latex %f && latex %f"))
;; Add notes refile file for fast captures


;;;;
;; Ob
;;;;

;; ;; (require 'ob-julia)
;; ;; (load-file "~/.emacs.d/private/local/ob-julia/ob-julia.el")
;; (add-to-list 'load-path "~/.emacs.d/private/local/ob-julia/")
;; (load "ob-julia.el")
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((dot . t) (julia . t) (python . t)))

;; (setq org-confirm-babel-evaluate nil)
;; (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)   
;; (add-hook 'org-mode-hook 'org-display-inline-images)


(root-leader
  "a" 'org-agenda)



(provide 'init-org)
