

(use-package pdf-tools
  :ensure t
  :config
  (pdf-loader-install))


(use-package tex
  :ensure auctex
  :defer
  :defines TeX-view-program-list TeX-view-program-selection
  :init
  (setq TeX-debug-bad-boxes t
        TeX-parse-self t
        TeX-source-correlate-mode t
        TeX-auto-save t)
  :config
  (setq-default TeX-master nil)
  ;; (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
  ;; 	TeX-source-correlate-start-server t)
  (setq TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)) 
        TeX-view-program-selection '((output-pdf "PDF Tools"))  
        TeX-source-correlate-start-server t)
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  (add-hook 'LaTeX-mode-hook 'turn-off-auto-fill))

(use-package reftex
  :ensure t
  :diminish reftex-mode
  :hook (LaTeX-mode . turn-on-reftex)
  :init
  (setq reftex-plug-into-AUCTeX t
        reftex-ref-style-default-list '("Cleveref" "Varioref" "Default")))

(use-package latex-extra
  :ensure t
  :diminish latex-extra-mode
  :hook (LaTeX-mode . latex-extra-mode)
  :config
  (remove-hook 'latex-extra-mode-hook 'latex/setup-auto-fill))

(use-package company-auctex
  :ensure t
  :hook (TeX-mode . company-auctex-init))

(use-package auctex-latexmk
  :ensure t
  :hook (LaTeX-mode . auctex-latexmk-setup)
  :init
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))


(use-package writeroom-mode
  :ensure t)

(root-leader
  "w" 'writeroom-mode)


(provide 'init-tex)
