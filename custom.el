(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face 'default)
 '(custom-safe-themes
   '("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" default))
 '(fci-rule-color "#3C3D37")
 '(highlight-changes-colors '("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   '(("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100)))
 '(magit-diff-use-overlays nil)
 '(org-agenda-files
   '("/Users/matt/org/todo.org" "/Users/matt/org/archive.org" "/Users/matt/org/ideageneration.org" "/Users/matt/org/job_hunt.org" "/Users/matt/org/questions.org" "/Users/matt/org/refile.org" "/Users/matt/org/study.org" "/Users/matt/org/teaching.org" "/Users/matt/org/toread.org" "/Users/matt/org/plan/blog.org" "/Users/matt/org/plan/ideas.org" "/Users/matt/org/plan/organizing.org" "/Users/matt/org/plan/pact.org" "/Users/matt/org/plan/plan.org" "/Users/matt/org/plan/projects.org" "/Users/matt/org/projects/analyzing_rnns_in_rl.org" "/Users/matt/org/projects/comp_gvfs.org" "/Users/matt/org/projects/curiosity.org" "/Users/matt/org/projects/flux.org" "/Users/matt/org/projects/interview_study.org" "/Users/matt/org/projects/juliarl.org" "/Users/matt/org/projects/pred_reps.org" "/Users/matt/org/projects/predictive_coding.org" "/Users/matt/org/projects/reproduce.org" "/Users/matt/org/projects/sarah_forever.org" "/Users/matt/org/projects/website.org" "/Users/matt/org/notes/current_learning_objectives.org") nil nil "Customized with use-package org")
 '(package-selected-packages
   '(org org-roam zotxt yasnippet-snippets yaml-mode writeroom-mode which-key unfill undo-tree spaceline request-deferred refine rainbow-delimiters quelpa-use-package poly-markdown pipenv persp-mode pdf-tools ox-hugo org-roam-bibtex org-ref org-journal org-inline-pdf org-fragtog org-bullets neotree multiple-cursors monokai-theme magit lsp-ui lsp-julia latex-extra jupyter ivy-bibtex hl-todo helm-lsp god-mode general fullframe eyebrowse elpy eglot-jl dimmer diminish counsel-projectile company-quickhelp company-org-roam company-auctex company-anaconda biblio auto-compile auctex-latexmk))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(safe-local-variable-values
   '((org-confirm-babel-evaluate)
     (org-babel-default-header-args:julia
      (:session . "jl")
      (:kernel . "julia-orgroam")
      (:exports . "both"))
     (org-babel-default-header-args:jupyter-julia
      (:session . "jl")
      (:kernel . "julia-orgroam")
      (:exports . "both"))
     (eval setq org-download-image-dir
           (concat "./images/"
                   (file-name-base
                    (buffer-file-name))))
     (eval setq org-download-image-dir
           (concat "./images"
                   (file-name-base
                    (buffer-file-name))))
     (org-download-image-dir eval concat "./images"
                             (file-name-base
                              (buffer-file-name)))
     (org-download-image-dir concat "./images"
                             (file-name-base
                              (buffer-file-name)))
     (org-download-image-dir . "./images")
     (elisp-lint-indent-specs
      (describe . 1)
      (it . 1)
      (thread-first . 0)
      (cl-flet . 1)
      (cl-flet* . 1)
      (org-element-map . defun)
      (org-roam-dolist-with-progress . 2)
      (org-roam-with-temp-buffer . 1)
      (org-with-point-at . 1)
      (magit-insert-section . defun)
      (magit-section-case . 0)
      (org-roam-with-file . 2))
     (elisp-lint-ignored-validators "byte-compile" "package-lint")
     (eglot-jl-julia-command . "julia-1.9")
     (eglot-jl-julia-command . "julia-1.8")
     (eglot-jl-julia-command . "julia-1.7")
     (eglot-jl-julia-command . "julia-1.6")
     (eglot-jl-julia-command . "julia-1.5")
     (eglot-jl-julia-command . "julia-1.4")
     (TeX-master . t)
     (TeX-master . "main.org")
     (org-hugo-section . "braindump")
     (org-hugo-base-dir . "~/Documents/Professional/website/")
     (bibtex-completion-bibliography "./thesis.bib" "./me.bib")
     (bibtex-completion-bibliography quote
                                     ("./thesis.bib" "./me.bib"))
     (org-preview-latex-image-directory . "../notes-ltximg/")
     (bibtex-completion-bibliography . "./thesis.bib")
     (bibtex-completion-bibliography "./thesis.bib")
     (bibtex-completion-bibliography "./me.bib" "./thesis.bib")
     (julia-indent-offset . 2)
     (bibtex-completion-bibliography "./rnns.bib" "./rl.bib" "./me.bib" "./candidacy.bib" "./predreps.bib")
     (lsp-julia-command . "julia_1_6")
     (eglot-jl-julia-command . "julia_1_6")
     (eglot-jl-julia-command . "julia_1_5")
     (eglot-jl-julia-command . "julia_1_4")
     (org-notes-root-dir . "~/Google Drive/org/notes/general/")
     (org-notes-root-dir . "~/Documents/Research/notes/general/")
     (org-notes-parent-file . "../math.org")
     (org-notes-parent-file . "../linear_algebra.org")
     (org-notes-root-file . "../math.org")
     (org-notes-root-file . \../math.org)
     (eval setq flycheck-clang-include-path
           (list "/Users/Matt/Documents/Research/projects/GVF/inc" "/Users/Matt/Documents/Research/projects/GVF/lib" "/opt/local/include/libomp"))
     (company-clang-arguments "-I/Users/Matt/Documents/Research/projects/GVF/inc" "-I/Users/Matt/Documents/Research/projects/GVF/lib" "-I/opt/local/include/libomp" "-std=c++14")))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-roam-link ((t (:inherit org-link :foreground "#9c8321"))) t))
