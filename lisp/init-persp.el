
(root-leader
  "l" '(:ignore t :which-key "persp")
  "e" '(:ignore t :which-key "eyebrowse"))

(use-package refine
  :ensure t)

(use-package persp-mode
  :ensure t
  :hook ((after-init . persp-mode)
         (emacs-startup . toggle-frame-maximized))
  :defines ivy-sort-functions-alist
  :functions
  persp-reorder
  :init
  (defun persp-reorder ()
    (interactive)
    (refine 'persp-names-cache))
  :custom
  (persp-nil-name "default")
  (persp-set-last-persp-for-new-frames nil)
  (persp-auto-resume-time 0.1)
  (persp-autokill-buffer-on-remove t)
  :config

  (add-hook 'persp-common-buffer-filter-functions
            (lambda (b) (or (string-prefix-p "*" (buffer-name b))
			    (string-prefix-p "magit" (buffer-name b)))))
  
  (with-eval-after-load "ivy"
    (add-hook 'ivy-ignore-buffers
              #'(lambda (b)
                  (when persp-mode
                    (let ((persp (get-current-persp)))
                      (if persp
                          (not (persp-contain-buffer-p b persp))
                        nil)))))

    (setq ivy-sort-functions-alist
          (append ivy-sort-functions-alist
                  '((persp-kill-buffer   . nil)
                    (persp-remove-buffer . nil)
                    (persp-add-buffer    . nil)
                    (persp-switch        . nil)
                    (persp-window-switch . nil)
                    (persp-frame-switch  . nil)))))


  
  (with-eval-after-load "hydra"
    (bind-keys ("M-m l" .
                (lambda ()
                  (interactive)
                  (call-interactively
                   (eval `(defhydra mattroot/hydra-persp (:hint nil :exit t)
			    ;; Docstring
                            (concat "Select Perspective: "
				    "\n"
				    (mattroot/hydra-prepare-dynamic-names mattroot/hydra-dynamic-selectors
									  persp-names-cache
									  (safe-persp-name (get-frame-persp)))
				    "\n")
			    ;; Heads
                            ,@(mattroot/hydra-prepare-dynamic-heads mattroot/hydra-dynamic-selectors
								    persp-names-cache
								    'persp-frame-switch)
			    ("n" (progn
				   (persp-next)
				   (message "Current Perspective: %s" (safe-persp-name (get-frame-persp))))
			     "next" :color pink)
			    ("p" (progn
				   (persp-prev)
				   (message "Current Perspective: %s" (safe-persp-name (get-frame-persp))))
			     "previous" :color pink)
			    ("c" (progn
				   (interactive)
				   (let ((new-persp (read-string "A name for the new perspective: ")))
				     (persp-add-new new-persp)
				     (persp-frame-switch new-persp)
				     (message "Created new perspective %s" new-persp))
				   )
			     "create-and-switch")
			    ("k" persp-kill "kill")
			    ("r" persp-reorder "reorder")
			    ("a" (progn
				   (persp-add-buffer (current-buffer)))  "add-current-buffer")
                            ("q" nil "quit" :color blue)
			    )))))))
  )



(use-package eyebrowse :demand t
  :init
  ;; (setq eyebrowse-keymap-prefix (kbd "M-m e"))
  (global-unset-key (kbd "C-c C-w"))
  :functions
  mattroot/eyebrowse--get-tags
  mattroot/eyebrowse--get-slots
  mattroot/eyebrowse--get-current-tag
  mattroot/eyebrowse--get-current-slot-idx
  :custom
  (eyebrowse-wrap-around t)
  :config
  (eyebrowse-mode)
  (add-hook 'persp-before-switch-functions
	    #'workspaces/update-eyebrowse-for-perspective)
  (add-hook 'eyebrowse-post-window-switch-hook
	    #'workspaces/save-eyebrowse-for-perspective)
  (add-hook 'persp-activated-functions
	    #'workspaces/load-eyebrowse-for-perspective)
  (add-hook 'persp-before-save-state-to-file-functions #'workspaces/update-eyebrowse-for-perspective)
  (add-hook 'persp-after-load-state-functions #'workspaces/load-eyebrowse-after-loading-layout)
  
  (defun mattroot/eyebrowse--get-tags ()
    (--map
     (let ((split-tag (split-string (eyebrowse-format-slot it) ":")))
       (if (eq (length split-tag) 2)
	   (nth 1 split-tag)
	 (car split-tag)))
     (eyebrowse--get 'window-configs)))
  
  (defun mattroot/eyebrowse--get-slots ()
    (--map (int-to-string (car it))
           (eyebrowse--get 'window-configs)))

  (defun mattroot/eyebrowse--get-current-slot-idx ()
    (cl-position (number-to-string (eyebrowse--get 'current-slot)) (mattroot/eyebrowse--get-slots) :test 'equal))

  (defun mattroot/eyebrowse--get-current-tag ()
    (let* ((current-config (nth (mattroot/eyebrowse--get-current-slot-idx) (eyebrowse--get 'window-configs)))
	   (split-tag (split-string (eyebrowse-format-slot current-config) ":")))
      (if (eq (length split-tag) 2)
  	  (nth 1 split-tag)
  	(car split-tag))))

  (mattroot/hydra-prepare-dynamic-names (mapconcat 'identity (mattroot/eyebrowse--get-slots) "")
					(mattroot/eyebrowse--get-tags)
					(mattroot/eyebrowse--get-current-tag))

  (with-eval-after-load "hydra"
    (bind-keys ("M-m e" .
		(lambda ()
		  (interactive)
		  (call-interactively
		   (eval `(defhydra mattroot/hydra-eyebrowse (:color blue :hint nil)
			    (concat
			     "Current Slots: "
			     (mattroot/hydra-prepare-dynamic-names (mapconcat 'identity (mattroot/eyebrowse--get-slots) "")
								   (mattroot/eyebrowse--get-tags)
								   (mattroot/eyebrowse--get-current-tag))
			     "
^Navigate^          ^Modify^
^^^^^^^----------------------------
_p_ -> previous     _k_ -> kill 
_n_ -> next         _c_ -> choose
_l_ -> last         _r_ -> rename     _q_ quit")
			    ("p" (progn
				   (eyebrowse-prev-window-config nil)
				   (message "%s" (mattroot/eyebrowse--get-current-tag)))
			     :exit nil)
			    ("l" #'eyebrowse-last-window-config)
			    ("n" (progn
				   (eyebrowse-next-window-config nil)
				   (message "%s" (mattroot/eyebrowse--get-current-tag)))
			     :exit nil)
			    ("k" #'eyebrowse-close-window-config :exit nil)
			    ("c" #'eyebrowse-switch-to-window-config)
			    ("r" #'eyebrowse-rename-window-config :color blue)
			    ("q" nil)
			    ("0" #'eyebrowse-switch-to-window-config-0 :color blue)
			    ("1" #'eyebrowse-switch-to-window-config-1 :color blue)
			    ("2" #'eyebrowse-switch-to-window-config-2 :color blue)
			    ("3" #'eyebrowse-switch-to-window-config-3 :color blue)
			    ("4" #'eyebrowse-switch-to-window-config-4 :color blue)
			    ("5" #'eyebrowse-switch-to-window-config-5 :color blue)
			    ("6" #'eyebrowse-switch-to-window-config-6 :color blue)
			    ("7" #'eyebrowse-switch-to-window-config-7 :color blue)
			    ("8" #'eyebrowse-switch-to-window-config-8 :color blue)
			    ("9" #'eyebrowse-switch-to-window-config-9 :color blue))
			 )))))))

;;;;;
;; Bridge for eyebrowse and persp-mode
;; Code from https://gist.github.com/gilbertw1/8d963083efea41f28bfdc85ed3c93eb4
;;;;;

(defun workspaces/get-persp-workspace (&optional persp frame)
  "Get the correct workspace parameters for perspective.
PERSP is the perspective, and defaults to the current perspective.
FRAME is the frame where the parameters are expected to be used, and
defaults to the current frame."
  (let ((param-names (if (display-graphic-p frame)
                         '(gui-eyebrowse-window-configs
                           gui-eyebrowse-current-slot
                           gui-eyebrowse-last-slot)
                       '(term-eyebrowse-window-configs
                         term-eyebrowse-current-slot
                         term-eyebrowse-last-slot))))
    (--map (persp-parameter it persp) param-names)))

(defun workspaces/set-persp-workspace (workspace-params &optional persp frame)
  "Set workspace parameters for perspective.
WORKSPACE-PARAMS should be a list containing 3 elements in this order:
- window-configs, as returned by (eyebrowse--get 'window-configs)
- current-slot, as returned by (eyebrowse--get 'current-slot)
- last-slot, as returned by (eyebrowse--get 'last-slot)
PERSP is the perspective, and defaults to the current perspective.
FRAME is the frame where the parameters came from, and defaults to the
current frame.
Each perspective has two sets of workspace parameters: one set for
graphical frames, and one set for terminal frames."
  (let ((param-names (if (display-graphic-p frame)
                         '(gui-eyebrowse-window-configs
                           gui-eyebrowse-current-slot
                           gui-eyebrowse-last-slot)
                       '(term-eyebrowse-window-configs
                         term-eyebrowse-current-slot
                         term-eyebrowse-last-slot))))
    (--zip-with (set-persp-parameter it other persp)
                param-names workspace-params)))

(defun workspaces/load-eyebrowse-for-perspective (type &optional frame)
  "Load an eyebrowse workspace according to a perspective's parameters.
 FRAME's perspective is the perspective that is considered, defaulting to
 the current frame's perspective.
 If the perspective doesn't have a workspace, create one."
  (when (eq type 'frame)
    (let* ((workspace-params (workspaces/get-persp-workspace (get-frame-persp frame) frame))
           (window-configs (nth 0 workspace-params))
           (current-slot (nth 1 workspace-params))
           (last-slot (nth 2 workspace-params)))
      (if window-configs
          (progn
            (eyebrowse--set 'window-configs window-configs frame)
            (eyebrowse--set 'current-slot current-slot frame)
            (eyebrowse--set 'last-slot last-slot frame)
            (eyebrowse--load-window-config current-slot))
        (eyebrowse--set 'window-configs nil frame)
        (eyebrowse-init frame)
        (workspaces/save-eyebrowse-for-perspective frame)))))

(defun workspaces/load-eyebrowse-after-loading-layout (_state-file _phash persp-names)
  "Bridge between `persp-after-load-state-functions' and
`workspaces/load-eyebrowse-for-perspective'.
_PHASH is the hash were the loaded perspectives were placed, and
PERSP-NAMES are the names of these perspectives."
  (let ((cur-persp (get-current-persp)))
    ;; load eyebrowse for current perspective only if it was one of the loaded
    ;; perspectives
    (when (member (or (and cur-persp (persp-name cur-persp))
                      persp-nil-name)
                  persp-names)
      (workspaces/load-eyebrowse-for-perspective 'frame))))
(defun workspaces/update-eyebrowse-for-perspective (&rest _args)
  "Update and save current frame's eyebrowse workspace to its perspective."
  (let* ((current-slot (eyebrowse--get 'current-slot))
         (current-tag (nth 2 (assoc current-slot (eyebrowse--get 'window-configs)))))
    (eyebrowse--update-window-config-element
     (eyebrowse--current-window-config current-slot current-tag)))
  (workspaces/save-eyebrowse-for-perspective))
(defun workspaces/save-eyebrowse-for-perspective (&optional frame)
  "Save FRAME's eyebrowse workspace to FRAME's perspective.
FRAME defaults to the current frame."
  (workspaces/set-persp-workspace (list (eyebrowse--get 'window-configs frame)
                                        (eyebrowse--get 'current-slot frame)
                                        (eyebrowse--get 'last-slot frame))
                                  (get-frame-persp frame)
				  frame))


;; (with-eval-after-load "persp-mode-autoloads"
;;   (setq wg-morph-on nil) ;; switch off animation
;;   (setq persp-autokill-buffer-on-remove 'kill-weak)
;;   (add-hook 'after-init-hook #'(lambda () (persp-mode ))))


;; (root-leader
;;   "l" '(:keymap persp-key-map :which-key "persp"))

(provide 'init-persp)
