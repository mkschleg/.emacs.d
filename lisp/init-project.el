


(use-package projectile
  :ensure t
  :custom
  (projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t)

(root-leader
  "p" '(:keymap projectile-command-map :which-key "projectile"))

