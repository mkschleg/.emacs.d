

;; Hydra: https://www.wisdomandwonder.com/article/10596/screencast-building-a-little-ui-to-manage-buffers


(root-leader
  "l" '(:ignore t :which-key "eyebrowse"))


(use-package desktop
  :hook ()
  :config
  (desktop-save-mode 1))

(use-package eyebrowse
  :ensure t
  :init
  (setq eyebrowse-keymap-prefix (kbd "M-m l"))
  (global-unset-key (kbd "C-c C-w"))
  ;; :bind-keymap
  ;; ("M-m l" . eyebrowse-mode-map)
  :config
  (eyebrowse-mode t)
  (setq eyebrowse-new-workspace t))


(provide 'init-eyebrowse)
