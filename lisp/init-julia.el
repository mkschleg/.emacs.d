
;; Name prefix for which key
(root-leader
    "j" '(:ignore t :which-key "jupyter"))

(use-package julia-mode
  :ensure t
  :custom
  (inferior-julia-program-name "julia"))

(use-package jupyter
  :ensure t
  :bind (("M-m j r" . jupyter-run-repl)
	 ("M-m j c" . jupyter-connect-repl)))

(provide 'init-julia)
