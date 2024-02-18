


;; (use-package projectile
;;   :ensure t
;;   :custom
;;   (projectile-completion-system 'ivy)
;;   (projectile-mode +1)
;;   :config
  
;;   (advice-add 'projectile-project-root :before-while
;;               (lambda (&optional dir)
;;                 (not (file-remote-p (or dir default-directory)))))
;;   )

;; (use-package counsel-projectile
;;   :ensure t)

;; (root-leader
;;   "r" '(:keymap projectile-command-map :which-key "projectile"))

(provide 'init-project)

