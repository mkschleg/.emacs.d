

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq load-prefer-newer t)

;; Potentially makes things snappier by doing garbage collection out of frame.
(add-hook 'focus-out-hook #'garbage-collect)

;;;;;;;;;
;; Initialize config
;;;;;;;;;

(setq ring-bell-function 'ignore)
(setq make-backup-files nil)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(defun reload-config ()
  "Reload the config for emacs."
  (interactive)
  (load-file "~/.emacs"))

;;;;;;
;; Core 
;;;;;;

(require 'init-package)
(require 'init-funcs)
(require 'init-keybinds)
(require 'init-visuals)
(require 'init-hydra)
(require 'init-persp)
(require 'init-ivy)
(require 'init-editor)



;;;;;;;;
;; Mode configs
;;;;;;;;
;; (require 'init-org)
;; (require 'init-tex)
(require 'init-julia)



