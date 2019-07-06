;;;;;;;;;;;;
;;
;; Helm config
;;
;;;;;;;;;;;;


(use-package helm
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x C-b" . helm-buffer-list))
  :bind-keymap
  ("M-m h" . helm-command-map)
  :ensure t)

(use-package helm-swoop
  :bind (("C-." . helm-swoop))
  :ensure t)

(general-define-key
 :prefix "C-x"
 "C-f" 'helm-find-files)

(root-leader
 "/" 'helm-swoop)

;; (require 'helm-persp-bridge)

(provide 'init-helm)
