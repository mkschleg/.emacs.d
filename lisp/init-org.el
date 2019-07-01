



(setq org-treat-S-cursor-todo-selection-as-state-change nil)
;; Ability to use key shortcuts for selecting a state
(setq org-use-fast-todo-selection t)
;; Calculate the statistics of a todo item recursively through the tree
(setq org-hierarchical-todo-statistics nil)
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

;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)





(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation))
(add-hook 'org-mode-hook 'org-mode-reftex-setup)

(setq org-latex-pdf-process (list "latexmk -pdf -bibtex %f"))
;; (setq org-latex-to-pdf-process '("latex %f && bibtex %f && latex %f && latex %f"))
;; Add notes refile file for fast captures

(setq org-default-notes-file (concat org-directory "/refile.org"))
(setq org-highlight-latex-and-related '(latex script entities))
;; (setq org-pretty-entities t)
;; (define-key global-map "\C-cc" 'org-capture)

(setq org-agenda-files (quote
                        ("~/org/")))

(setq org-agenda-tag-filter-preset (quote
                                    ("-ignore")))





(use-package org-bullets
  :ensure t
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

;;;;
;; zotxt
;;;;

;;;;
;; visual-fill-column
;;;;


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


