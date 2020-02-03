


(setq column-number-mode t)
(tool-bar-mode -1)
(toggle-scroll-bar -1)


(setq inhibit-startup-screen t)
(defalias 'yes-or-no-p 'y-or-n-p)


(add-to-list 'default-frame-alist '(font . "Hack-14"))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(horizontal-scroll-bars . nil))


(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))



(use-package spaceline
  :ensure t
  :config
  (setq powerline-image-apple-rgb t)
  (spaceline-spacemacs-theme))

;; (use-package powerline
;;   :ensure t
;;   :config
;;   (setq powerline-image-apple-rgb t)
;;   (powerline-default-theme))


(use-package dimmer
  :ensure t
  :config
  (dimmer-mode))

;;;;;
;; Change empty line bitmap to tilde (if active)
;;;;;
(progn
  (define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
  (setcdr (assq 'empty-line fringe-indicator-alist) 'tilde))

(provide 'init-visuals)
