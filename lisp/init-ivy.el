


(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-selectable-prompt t))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
	 ("C-s" . swiper)
	 ("C-x C-r" . counsel-recentf)
	 ("C-x C-f" . counsel-find-file)))

;;;;;;
;;
;; Get Ivy to play nice with persp-mode
;;
;;;;;;




(provide 'init-ivy)
