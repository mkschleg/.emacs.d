

;; (use-package elpy
;;   :ensure t
;;   :init
;;   (elpy-enable))

;; (use-package elpy
;;   :ensure t
;;   :init
;;   (elpy-enable 1))

(use-package anaconda-mode
  :ensure t
  :hook ((python-mode . anaconda-mode)
	 (python-mode . anaconda-eldoc-mode)))

(require 'rx)

(use-package company-anaconda
  :ensure t
  :init
  (add-to-list 'company-backends 'company-anaconda))

;; (use-package conda
;;   :ensure t
;;   :custom
;;   (conda-anaconda-home "~/opt/miniconda3/envs"))

(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" "~/opt/miniconda3/envs")
  (pyvenv-mode 1))


(provide 'init-python)
