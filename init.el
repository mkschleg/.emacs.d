

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq load-prefer-newer t)

(setq force-load-messages t)
(setq debug-on-error t)

(global-subword-mode 1)

(setq make-backup-files nil)

(add-hook 'focus-out-hook #'garbage-collect)


(defun reload-config ()
  "Reload the config for emacs."
  (interactive "r")
  (load-file ~/.emacs))

(require 'init-package)
(require 'init-ivy)
(require 'init-dashboard)
(require 'init-persp)
(require 'init-keybinds)



;;;;;;;;;;;;
;;
;; Misc Settings to be moved in the future.
;;
;;;;;;;;;;;;

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

(load-theme 'monokai t)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq column-number-mode t)
(require 'autopair)

(windmove-default-keybindings 'meta)

(tool-bar-mode -1)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; (use-package smart-mode-line
;;   :ensure t
;;   :config
;;   (setq powerline-arrow-shape 'curve)
;;   (setq powerline-default-separator-dir '(right . left))
;;   ;; These two lines you really need.
;;   (setq sml/theme 'powerline)
;;   (sml/setup))

(use-package spaceline
  :ensure t
  :config
  (spaceline-spacemacs-theme))

(toggle-scroll-bar -1)



;; (use-package powerline
;;   :ensure t
;;   :config
;;   (powerline-default-theme))
