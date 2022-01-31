

;; parse init.org with org-mode
;; (require 'org)
;; (org-babel-load-file
;;  (expand-file-name "init.org"
;;                    user-emacs-directory))

;;; parsed init.org with bespoke function from http://endlessparentheses.com/init-org-Without-org-mode.html
(defvar endless/init.org-message-depth 3
  "What depth of init.org headers to message at startup.")

(with-temp-buffer
  (insert-file "~/.emacs.d/init.org")
  (goto-char (point-min))
  (while (not (eobp))
    (forward-line 1)
    (cond
     ;; Report Headers
     ((looking-at
       (format "\\* +.*$" 
               endless/init.org-message-depth))
      (message "%s" (match-string 0)))
     ;; Evaluate Code Blocks
     ((looking-at "^#\\+BEGIN_SRC +emacs-lisp *$")
      (let ((l (match-end 0)))
        (search-forward "\n#+END_SRC")
        (eval-region l (match-beginning 0))))
     ;; Finish on the next level-1 header
     ((looking-at "^\\* ")
      (goto-char (point-max))))))

;; (with-temp-buffer
;;   (insert-file "~/.emacs.d/init.org")
;;   (goto-char (point-min))
;;   (search-forward "\n* init.el")
;;   (while (not (eobp))
;;     (forward-line 1)
;;     (cond
;;      ;; Report Headers
;;      ((looking-at
;;        (format "\\*\\{2,%s\\} +.*$" 
;;                endless/init.org-message-depth))
;;       (message "%s" (match-string 0)))
;;      ;; Evaluate Code Blocks
;;      ((looking-at "^#\\+BEGIN_SRC +emacs-lisp *$")
;;       (let ((l (match-end 0)))
;;         (search-forward "\n#+END_SRC")
;;         (eval-region l (match-beginning 0))))
;;      ;; Finish on the next level-1 header
;;      ((looking-at "^\\* ")
;;       (goto-char (point-max))))))


;; (when (string= system-type "darwin")       
;;   (setq dired-use-ls-dired nil))

;; (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (setq load-prefer-newer t)

;; ;; Potentially makes things snappier by doing garbage collection out of frame.
;; (add-hook 'focus-out-hook #'garbage-collect)

;; ;;;;;;;;;
;; ;; Initialize config
;; ;;;;;;;;;

;; (setq ring-bell-function 'ignore)
;; (setq make-backup-files nil)

;; (setq custom-file "~/.emacs.d/custom.el")
;; (load custom-file 'noerror)

;; (defun reload-config ()
;;   "Reload the config for emacs."
;;   (interactive)
;;   (load-file "~/.emacs"))


;; ;;;;;;
;; ;; Core 
;; ;;;;;;

;; (require 'init-package)
;; (require 'init-funcs)
;; (require 'init-keybinds)
;; (require 'init-visuals)
;; (require 'init-hydra)
;; (require 'init-persp)
;; (require 'init-ivy)
;; (require 'init-editor)
;; (require 'init-project)

;; ;;;;;;;;
;; ;; Mode configs
;; ;;;;;;;;
;; (require 'init-org)
;; (require 'init-tex)
;; (require 'init-julia)
;; (require 'init-python)


;; ;;;;;;;;
;; ;; Extras
;; ;;;;;;;;
;; (require 'init-org-notes)
;; ;; (require 'init-org-papers)
;; ;; (require 'init-org-daily)
;; ;; (require 'init-brain)
