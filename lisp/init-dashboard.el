

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(dashboard-modify-heading-icons '((recents . "file-text")
                                  (bookmarks . "book")))

(provide 'init-dashboard)
