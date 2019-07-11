






(use-package org
  :functions
  org-renumber-environment
  :bind (("C-c a" . org-agenda))
  :custom
  (org-directory "~/org/")
  (org-default-notes-file (concat org-directory "/refile.org"))
  (org-highlight-latex-and-related '(latex script entities))
  (org-agenda-files (list "~/org/"
			  "~/Documents/Research/notes/projects/"
			  "~/Documents/Research/notes/topics/"))
  (org-agenda-tag-filter-preset (quote
                                 ("-ignore")))
  (org-treat-S-cursor-todo-selection-as-state-change nil)
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
  :config

  ;; Adding some renumbering functionality for equations in org files
  
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

  (advice-add 'org-create-formula-image :around #'org-renumber-environment)
  
  (defun mattroot/org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
    (unless mattroot/org-pub-dir
      (setq mattroot/org-pub-dir "exported-org-files")
      (unless (file-directory-p mattroot/org-pub-dir)
	(make-directory mattroot/org-pub-dir)))
    (apply orig-fun extension subtreep mattroot/org-pub-dir nil))
  (advice-add 'org-export-output-file-name :around #'mattroot/org-export-output-file-name-modified)

  (setcdr (assoc "\\.pdf\\'" org-file-apps) 'pdf-tools)

  (require 'init-org-macros)

  (setq org-export-global-macros mattroot/org-macros)

  (global-unset-key (kbd "C-c ["))

  )



;; (use-package org-publish
;;   :config
;;   (setq org-publish-project-alist
;;   '(("html"
;;      :base-directory "~/org/"
;;      :base-extension "org"
;;      :publishing-directory "~/org/exports"
;;      :publishing-function org-html-publish-to-html)
;;     ("pdf"
;;      :base-directory "~/org/"
;;      :base-extension "org"
;;      :publishing-directory "~/org/exports"
;;      :publishing-function org-latex-publish-to-pdf)
;;     ("all" :components ("html" "pdf"))))

;;   )


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
    (s-replace-all '((" " . "_") (".pdf" . ".org"))
  		   (nth 0 (last (split-string item "/")))))

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


;; (root-leader
;;   "a" 'org-agenda)



(provide 'init-org)
