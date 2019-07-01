


(use-package ivy
  :ensure t
;;  :bind (
;;	 ("M-x" . counsel-M-x)
;;	 ("C-c C-r" . ivy-resume)
;;	 ("\C-s" . swiper))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)))

;;(use-package swipper
;;  :ensure t
;;  :bind (("\C-s" . swiper)))



(provide 'init-ivy)
