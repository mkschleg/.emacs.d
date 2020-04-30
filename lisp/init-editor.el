

(windmove-default-keybindings 'meta)

(setq-default indicate-empty-lines t)
(global-subword-mode 1)
(diminish 'subword-mode)

(global-visual-line-mode)
(diminish 'visual-line-mode)

(diminish 'auto-rev-mode)

(setq indent-tabs-mode nil)

(use-package unfill
  :ensure
  :bind ("M-m u" . unfill-paragraph))


;;;
;; Delimiters
;;;

(show-paren-mode 1)

(use-package autopair
  :ensure t
  :diminish autopair-mode
  :config
  (autopair-global-mode)
  )

(use-package rainbow-delimiters
  :diminish rainbow-delimiters-mode
  :ensure t
  :hook (prog-mode . (lambda () (rainbow-delimiters-mode))))

(use-package flyspell
  :ensure t
  :diminish flyspell-mode
  :hook (text-mode . (lambda () (flyspell-mode 1))))

(use-package company
  :ensure t
  :config
  (global-company-mode))

(use-package company-quickhelp
  :ensure t
  :config
  (eval-after-load 'company
    '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)))

(use-package hl-line
  :ensure t
  :hook (prog-mode . hl-line-mode))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)
	 ("C-S-c C-S-c" . mc/edit-lines)))

(use-package ibuffer
  :bind ([remap list-buffers] . ibuffer)
  :config (fullframe ibuffer ibuffer-quit))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package undo-tree
  :ensure t
  :bind (("C-/" . undo)
	 ("C-?" . redo))
  :config
  (global-undo-tree-mode 1))

(use-package yasnippet
  :ensure t)

(use-package yasnippet-snippets
  :ensure t)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deffered))

(use-package lsp-ui :ensure t :commands lsp-ui-mode)
(use-package company-lsp :ensure t :commands company-lsp)
(use-package helm-lsp :ensure t :commands helm-lsp-workspace-symbol)

;; (diminish 'eldoc-mode)

(use-package hl-todo
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'hl-todo-mode))


(remove-hook 'LaTeX-mode-hook 'latex/auto-fill-mode)
  ;; (add-hook 'LaTeX-mode-hook 'visual-line-mode)

(use-package yaml-mode
  :ensure t)

(root-leader
  "n" "neotree")

(use-package neotree
  :ensure t
  :bind (("M-m n" . neotree))
  :config (setq neo-smart-open t))

;; Javascript tab level
(setq js-indent-level 2)
;; (add-hook 'json-mode-hook
;;           (lambda ()
;;             (make-local-variable 'js-indent-level)
;;             (setq js-indent-level 2)))

(provide 'init-editor)
