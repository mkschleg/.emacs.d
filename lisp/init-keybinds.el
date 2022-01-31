

;; (use-package god-mode
;;   :ensure t
;;   :bind (("<escape>" . god-local-mode)))

(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0.25)
  (which-key-mode)
  (which-key-enable-god-mode-support)
  (diminish 'which-key-mode))


(use-package general
  :ensure t)

;; Reset some keys
(general-define-key
 "M-m" nil)

;; Some basic key defines
(general-define-key
 "M-a" 'back-to-indentation)


(general-create-definer root-leader
  :prefix "M-m")

(root-leader
  ;; :prefix my-leader
  ;; or without a variable
  "" '(nil :which-key "Root Leader")
  "c" '(:ignore t :which-key "comments"))


(root-leader
  "cl" 'comment-line
  "cd" 'comment-dwim
  "cr" 'comment-or-uncomment-region)



(provide 'init-keybinds)


