

;; (use-package elpy
;;   :straight t
;;   :init
;;   (elpy-enable))

;; (use-package 



;; (use-package elpy
;;   :straight t
;;   :init
;;   (elpy-enable 1))

;; (use-package anaconda-mode
;;   :straight t
;;   :hook ((python-mode . anaconda-mode)
;; 	 (python-mode . anaconda-eldoc-mode)))

(require 'rx)

;; (use-package company-anaconda
;;   :straight t
;;   :init
;;   (add-to-list 'company-backends 'company-anaconda))

;; (use-package conda
;;   :straight t
;;   :custom
;;   (conda-anaconda-home "~/opt/miniconda3/envs"))

(use-package pyvenv
  :straight t
  :init
  ;; (setenv "WORKON_HOME" "~/opt/miniconda3/envs")
  (setenv "WORKON_HOME" "~/.pyenv/shims/")
  (pyvenv-mode 1))

;; (use-package pipenv
;;   :straight t
;;   :hook (python-mode . pipenv-mode)
;;   :init
;;   (setq
;;    pipenv-projectile-after-switch-function
;;    #'pipenv-projectile-after-switch-extended))

(provide 'init-python)
