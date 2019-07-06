

(use-package dashboard
  :ensure t
  :hook (after-init . dashboard-setup-startup-hook)
  :config
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))
  (setq dashboard-items '((recents  . 5)
			  (bookmarks . 5)
                        (projects . 5)))
  (dashboard-setup-startup-hook))



(provide 'init-dashboard)
