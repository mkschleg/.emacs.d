
(require 'package)

;;; Standard package repositories

(setq package-archives
      `(("melpa-stable" . "https://stable.melpa.org/packages/")
        ;; ("marmalade" . "https://marmalade-repo.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        . ,package-archives))

(setq package-archive-priorities
      '(("melpa-stable" . 100)
        ("marmalade" . 50)
        ("gnu" . 10)
        ("melpa" . 150)))

(setq package-enable-at-startup nil)
(package-initialize)

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'use-package) (package-install 'use-package))

(eval-when-compile (require 'use-package))

(use-package auto-compile
  :ensure
  :hook ((after-init . auto-compile-on-load-mode)
         (after-init . auto-compile-on-save-mode)))

;; (use-package bind-key :ensure)
(use-package diminish :ensure)
(use-package fullframe :ensure)

(provide 'init-package)
