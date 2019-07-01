;;;;;;;;;;;;
;;
;; Helm config
;;
;;;;;;;;;;;;


;;(defun custom-helm-config ()
;;  (global-set-key (kbd "M-x") 'helm-M-x))

(use-package helm
  :bind (("M-x" . helm-M-x))
  :ensure t)


(use-package helm-swoop
  :ensure t)

(provide 'init-helm)
