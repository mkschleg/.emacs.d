(with-eval-after-load "persp-mode"
  (with-eval-after-load "helm-mode"
    (defun helm-buffers-toggle-persp-filter ()
      (interactive)
      (with-helm-alive-p
        (let ((filter-attrs (helm-attr 'candidate-transformer
                                       helm-source-persp-buffers)))
          (if (memq #'helm-persp-buffers-filter-transformer filter-attrs)
              (progn
                (helm-attrset 'candidate-transformer
                              (delq #'helm-persp-buffers-filter-transformer
                                    filter-attrs)
                              helm-source-persp-buffers t)
                (helm-attrset 'name
                              "Buffers"
                              helm-source-persp-buffers t)
                (setq helm-persp-filtered-buffers-cache nil))
            (helm-attrset 'candidate-transformer
                          (cons #'helm-persp-buffers-filter-transformer
                                filter-attrs)
                          helm-source-persp-buffers t)
            (helm-attrset 'name
                          "Current perspective buffers"
                          helm-source-persp-buffers t))
          (helm-force-update))))
    (put 'helm-buffers-toggle-persp-filter 'helm-only t)
    (define-key helm-buffer-map
      persp-toggle-read-persp-filter-keys #'helm-buffers-toggle-persp-filter)
    (defvar helm-persp-filtered-buffers-cache nil)
    (defun helm-persp-buffers-filter-transformer (candidates)
      (setq helm-persp-filtered-buffers-cache nil)
      (let* ((persp (get-current-persp))
             (ret
              (cl-remove-if-not #'(lambda (bn)
                                    (let* ((ret (persp-contain-buffer-p (get-buffer bn) persp)))
                                      (unless ret
                                        (push bn helm-persp-filtered-buffers-cache))
                                      ret))
                                candidates)))
        ret))
    (defclass helm-persp-buffers-source (helm-source-buffers)
      ((buffer-list
        :initarg :buffer-list
        :initform #'(lambda () (mapcar #'buffer-name (buffer-list)))
        :custom function
        :documentation
        " A function with no arguments to create buffer list.")
       (cleanup :initform #'(lambda () (setq helm-persp-filtered-buffers-cache nil
                                        helm-buffers-list-cache nil)))
       (candidate-transformer :initform '(helm-persp-buffers-filter-transformer))))
    (defvar helm-source-persp-buffers
      (helm-make-source "Current perspective buffers"
          'helm-persp-buffers-source
        :fuzzy-match t))
    (defclass helm-persp-filtered-buffers-source (helm-source-buffers)
      ((candidates :initform #'(lambda ()
                                 (if helm-persp-filtered-buffers-cache
                                     helm-persp-filtered-buffers-cache
                                   (setq helm-persp-filtered-buffers-cache
                                         (mapcar #'buffer-name (persp-buffer-list-restricted nil 1))))))
       (cleanup :initform #'(lambda () (setq helm-persp-filtered-buffers-cache nil)))))
    (defvar helm-source-persp-filtered-buffers
      (helm-make-source "Other buffers"
          'helm-persp-filtered-buffers-source
        :fuzzy-match t))
    (defun helm-persp-buffer-list-bridge
        (prompt _collection &optional test _require-match init hist default _inherit-im name buffer)
      (let ((dflt (or default ""))
            (cbuf (current-buffer))
            helm-candidate-number-limit)
        (or
         (helm :sources (cond
                         ((eq this-command 'persp-add-buffer) '(helm-source-persp-filtered-buffers))
                         ((eq this-command 'persp-remove-buffer) '(helm-source-persp-buffers))
                         (t '(helm-source-persp-buffers helm-source-persp-filtered-buffers)))
               :fuzzy-match helm-mode-fuzzy-match
               :prompt prompt
               :buffer buffer
               :input init
               :history hist
               :resume 'noresume
               :keymap helm-buffer-map
               :truncate-lines helm-buffers-truncate-lines
               :default dflt
               :preselect #'(lambda ()
                              (if (and (eq this-command 'persp-temporarily-display-buffer)
                                       (persp-contain-buffer-p cbuf))
                                  (helm-next-source)
                                (helm-mark-current-line)
                                (let ((buffer-name-truncated-regexp (helm-buffers--quote-truncated-buffer cbuf))
                                      (start (point)) mp)
                                  (helm-awhile (re-search-forward buffer-name-truncated-regexp nil t)
                                    (when (helm-pos-header-line-p) (forward-line 1))
                                    (helm-mark-current-line)
                                    (when (eq cbuf (helm-get-selection)) (cl-return (setq mp it))))
                                  (goto-char (or mp start))
                                  (helm-mark-current-line)))))
         (helm-mode--keyboard-quit))))
    (defvar helm-persp-mini-default-sources
      (cons 'helm-source-persp-buffers
            (cons 'helm-source-persp-filtered-buffers
                  (remove 'helm-source-buffers-list helm-mini-default-sources))))
    (defun helm-persp-mini ()
      (interactive)
      (let* ((cbuf (current-buffer))
             (cbuf-name (buffer-name cbuf))
             helm-candidate-number-limit)
        (or
         (helm :sources helm-persp-mini-default-sources
               :ff-transformer-show-only-basename nil
               :fuzzy-match helm-mode-fuzzy-match
               :buffer "*helm persp mini*"
               :keymap helm-buffer-map
               :truncate-lines helm-buffers-truncate-lines
               :default cbuf-name
               :preselect #'(lambda ()
                              (let ((buffer-name-truncated-regexp (helm-buffers--quote-truncated-buffer cbuf))
                                    (start (point)) mp)
                                (helm-awhile (re-search-forward buffer-name-truncated-regexp nil t)
                                  (when (helm-pos-header-line-p) (forward-line 1))
                                  (helm-mark-current-line)
                                  (when (eq cbuf (helm-get-selection)) (cl-return (setq mp it))))
                                (goto-char (or mp start))
                                (helm-mark-current-line))))
         (helm-mode--keyboard-quit))))
    (global-set-key (kbd "C-x b") #'helm-persp-mini)
    (setq helm-completing-read-handlers-alist
          (append '((switch-to-buffer                 . helm-persp-buffer-list-bridge)
                    (persp-switch-to-buffer           . helm-persp-buffer-list-bridge)
                    (kill-buffer                      . helm-persp-buffer-list-bridge)
                    (persp-kill-buffer                . helm-persp-buffer-list-bridge)
                    (persp-temporarily-display-buffer . helm-persp-buffer-list-bridge)
                    (persp-add-buffer                 . helm-persp-buffer-list-bridge)
                    (persp-remove-buffer              . helm-persp-buffer-list-bridge))
                  helm-completing-read-handlers-alist))))

(provide 'helm-persp-bridge)
