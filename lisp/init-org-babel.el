





;; (use-package 
;;   :ensure t
;;   :custom

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((emacs-lisp . t)
;;    (python . t)))
;;   )

;; (use-package ob-julia
;;   :ensure t)
(use-package ob-async
  :ensure t)

(use-package jupyter
  :ensure t
  :after (org ob-async)
  :config
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (julia . t)
     (jupyter . t)))

  (org-babel-jupyter-override-src-block "julia")

  (setq ob-async-no-async-languages-alist '("jupyter-python" "jupyter-julia" "julia"))
)



(defun mattroot/ob-create-plot-file (suffix)
  (concat "./images/" (file-name-base (buffer-file-name)) "-" suffix)
  )
  


(provide 'init-org-babel)
