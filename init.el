

;; parse init.org with org-mode
;; (require 'org)
;; (org-babel-load-file
;;  (expand-file-name "init.org"
;;                    user-emacs-directory))

;;; parsed init.org with bespoke function from http://endlessparentheses.com/init-org-Without-org-mode.html
(defvar endless/init.org-message-depth 3
   "What depth of init.org headers to message at startup.")

(with-temp-buffer
  (insert-file "~/.emacs.d/init.org")
  (goto-char (point-min))
  (while (not (eobp))
    (forward-line 1)
    (cond
     ;; Report Headers
     ((looking-at
       (format "\\* +.*$" 
               endless/init.org-message-depth))
      (message "%s" (match-string 0)))
     ;; Evaluate Code Blocks
     ((looking-at "^#\\+BEGIN_SRC +emacs-lisp *$")
      (let ((l (match-end 0)))
        (search-forward "\n#+END_SRC")
        (eval-region l (match-beginning 0))))
     ;; Finish on the next level-1 header
     ((looking-at "^\\* ")
      (goto-char (point-max))))))




;; (when (string= system-type "darwin")       
;;   (setq dired-use-ls-dired nil))

;; (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (setq load-prefer-newer t)

;; ;; Potentially makes things snappier by doing garbage collection out of frame.
;; (add-hook 'focus-out-hook #'garbage-collect)

;; ;;;;;;;;;
;; ;; Initialize config
;; ;;;;;;;;;

;; (setq ring-bell-function 'ignore)
;; (setq make-backup-files nil)

;; (setq custom-file "~/.emacs.d/custom.el")
;; (load custom-file 'noerror)

;; (defun reload-config ()
;;   "Reload the config for emacs."
;;   (interactive)
;;   (load-file "~/.emacs"))


;; ;;;;;;
;; ;; Core 
;; ;;;;;;

;; (require 'init-package)
;; (require 'init-funcs)
;; (require 'init-keybinds)
;; (require 'init-visuals)
;; (require 'init-hydra)
;; (require 'init-persp)
;; (require 'init-ivy)
;; (require 'init-editor)
;; (require 'init-project)

;; ;;;;;;;;
;; ;; Mode configs
;; ;;;;;;;;
;; (require 'init-org)
;; (require 'init-tex)
;; (require 'init-julia)
;; (require 'init-python)


;; ;;;;;;;;
;; ;; Extras
;; ;;;;;;;;
;; (require 'init-org-notes)
;; ;; (require 'init-org-papers)
;; ;; (require 'init-org-daily)
;; ;; (require 'init-brain)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org org-roam zotxt yasnippet-snippets yaml-mode writeroom-mode which-key unfill undo-tree spaceline request-deferred refine rainbow-delimiters quelpa-use-package poly-markdown pipenv persp-mode pdf-tools ox-hugo org-roam-bibtex org-ref org-journal org-inline-pdf org-fragtog org-bullets neotree multiple-cursors monokai-theme magit lsp-ui lsp-julia latex-extra jupyter ivy-bibtex hl-todo helm-lsp god-mode general fullframe eyebrowse elpy eglot-jl dimmer diminish counsel-projectile company-quickhelp company-org-roam company-auctex company-anaconda biblio auto-compile auctex-latexmk)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
