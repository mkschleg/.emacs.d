

(use-package hydra
  :ensure t
  :functions
  mattroot/hydra-prepare-dynamic-names
  mattroot/hydra-prepare-dynamic-heads
  :defines mattroot/hydra-dynamic-selectors
  :config

  (setq mattroot/hydra-dynamic-selectors
	"0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

  
  (defun mattroot/hydra-prepare-dynamic-names (keys names &optional highlight-name highlight-list)
    (unless highlight-list (setq highlight-list '(:foreground "red")))
    ;; (message highlight-list)
    (mapconcat 'identity
	       (mapcar* (lambda (a b)
			  (if (string= b highlight-name)
			      (propertize (concat " [ _" (char-to-string a) "_ " b " ] ") 'face highlight-list)
			      ;; (concat "* _" (char-to-string a) "_ " b)
			    (concat " _" (char-to-string a) "_ " b " ")))
			keys names)
	       " | "))
  
  (defun mattroot/hydra-prepare-dynamic-heads (keys names func)
    (mapcar* (lambda (a b)
	       ;; (message "(mattroot/number-names persp-names-cache)")
	       (list (char-to-string a)
                     ;; `(sb/load-theme ',b)
		     `(funcall ',func ',b)
		     ;; `(message (format "hello %s" ',b))
                     ))
             keys names))

  )




(provide 'init-hydra)

