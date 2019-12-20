
;; Name prefix for which key
(root-leader
    "j" '(:ignore t :which-key "jupyter"))

(use-package julia-mode
  :ensure t
  :config
  (setq inferior-julia-program-name "julia")
  )



(use-package jupyter
  :ensure t
  :bind (("M-m j r" . jupyter-run-repl)
	 ("M-m j c" . jupyter-connect-repl))
  :config
  (org-babel-jupyter-override-src-block "python")
  (org-babel-jupyter-override-src-block "julia"))

;; (use-package eglot
;;   :ensure t)

;; (use-package eglot-jl
;;   :quelpa (eglot-jl :fetcher github :repo "non-Jedi/eglot-jl" :files ("*.el" "*.jl" "*.toml"))
;;   :config
;;   (add-hook 'julia-mode-hook 'eglot)
;;   (setq eglot-connect-timeout 300))

;; (use-package lsp-mode)

;; (use-package lsp-julia
;;   ;; :quelpa (lsp-julia :fetcher github :repo "non-Jedi/lsp-julia")
;;   :config
;;   (add-hook 'julia-mode-hook #'lsp-mode))

;; (use-package ob-julia
;;   :ensure t
;;   :quelpa
;;   (ob-julia :fetcher github :repo "gjkerns/ob-julia.git")
;;   :custom
;;   (org-babel-julia-command "julia -e 'include(\"$(ENV[\"HOME\"])/.julia/config/startup_babel.jl\")' -i")
;;   :config
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((dot . t) (julia . t) (python . t)))
;;   (setq org-confirm-babel-evaluate nil)
;;   )

;; (use-package ess
;;   :ensure t
;;   :init (require 'ess-site)
;;   :config
;;   (setq inferior-julia-args "")
;;   (setq ess-use-eldoc t)
;;   (setq ess-use-auto-complete t)
;;   (setq ess-tab-complete-in-script t))


(provide 'init-julia)
