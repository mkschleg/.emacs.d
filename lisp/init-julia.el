
;; Name prefix for which key
(root-leader
  "j" '(:ignore t :which-key "jupyter"))

(setq julia-default-exequtable "julia_1_6")

(use-package julia-mode
  :ensure t
  :config
  (setq inferior-julia-program-name "julia")
  (define-hostmode poly-julia-hostmode
    :mode 'julia-mode)

  ;; (font-lock-add-keywords 'julia-mode
  ;;                         '(("\\")))
  
  ;; (define-innermode poly-markdown-julia-comment-innermode
  ;;   :mode 'markdown-mode
  ;;   :head-matcher "[\s-]#=[\s-]MD"
  ;;   :tail-matcher "[\s-]=#[\s]*"
  ;;   :head-mode 'host
  ;;   :tail-mode 'host
  ;;   :keep-in-mode 'host
  ;;   :fallback-mode 'host)

  ;; (define-polymode poly-julia-mode
  ;;   :hostmode 'poly-julia-hostmode
  ;;   :innermodes '(poly-markdown-julia-comment-innermode))
  )

(use-package jupyter
  :ensure t
  :bind (("M-m j r" . jupyter-run-repl)
	 ("M-m j c" . jupyter-connect-repl))
  :config
  (org-babel-jupyter-override-src-block "python")
  (org-babel-jupyter-override-src-block "julia"))


(use-package eglot
  :ensure t
  :bind (("M-m g g" . eglot)))

(use-package eglot-jl
  :ensure t
  :config
  (setq eglot-connect-timeout 300)
  (eglot-jl-init)
  (add-to-list 'safe-local-variable-values '(eglot-jl-julia-command . "julia-1.4"))
  (add-to-list 'safe-local-variable-values '(eglot-jl-julia-command . "julia-1.5"))
  (add-to-list 'safe-local-variable-values '(eglot-jl-julia-command . "julia-1.6"))
  (add-to-list 'safe-local-variable-values '(eglot-jl-julia-command . "julia-1.7"))
  (add-to-list 'safe-local-variable-values '(eglot-jl-julia-command . "julia-1.8"))
  (add-to-list 'safe-local-variable-values '(eglot-jl-julia-command . "julia-1.9")))

;; (use-package lsp-julia
;;   :ensure t
;;   :custom
;;   (lsp-julia-default-environment "~/.julia/environments/v1.6")
;;   :config
;;   (add-to-list 'safe-local-variable-values '(lsp-julia-command . "julia_1_6")))


(use-package vterm
  :ensure t)
;; Now run `M-x vterm` and make sure it works!

;; (use-package julia-snail
;;   :ensure t
;;   :hook (julia-mode . julia-snail-mode)
;;   :custom
;;   (julia-snail-executable "julia-1.8"))



(provide 'init-julia)
