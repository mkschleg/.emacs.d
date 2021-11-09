


(use-package projectile
  :ensure t
  :custom
  (projectile-completion-system 'ivy)
  (projectile-mode +1))

(use-package counsel-projectile
  :ensure t)

(root-leader
  "r" '(:keymap projectile-command-map :which-key "projectile"))

(provide 'init-project)

