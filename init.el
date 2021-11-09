
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
(require 'init-project)

;;;;;;;;
;; Mode configs
;;;;;;;;
;; (require 'init-org)
(require 'init-tex)
(require 'init-julia)
(require 'init-python)


;;;;;;;;
;; Extras
;;;;;;;;
;; (require 'init-org-notes)
;; (require 'init-org-papers)
;; (require 'init-org-daily)
;; (require 'init-brain)
