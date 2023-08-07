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
   '("/Users/Matt/org/archive.org" "/Users/Matt/org/job_hunt.org" "/Users/Matt/org/questions.org" "/Users/Matt/org/refile.org" "/Users/Matt/org/study.org" "/Users/Matt/org/teaching.org" "/Users/Matt/org/todo.org" "/Users/Matt/org/toread.org" "/Users/Matt/org/plan/blog.org" "/Users/Matt/org/plan/ideas.org" "/Users/Matt/org/plan/organizing.org" "/Users/Matt/org/plan/pact.org" "/Users/Matt/org/plan/plan.org" "/Users/Matt/org/plan/projects.org" "/Users/Matt/org/projects/analyzing_rnns_in_rl.org" "/Users/Matt/org/projects/comp_gvfs.org" "/Users/Matt/org/projects/curiosity.org" "/Users/Matt/org/projects/interview_study.org" "/Users/Matt/org/projects/juliarl.org" "/Users/Matt/org/projects/pred_reps.org" "/Users/Matt/org/projects/predictive_coding.org" "/Users/Matt/org/projects/reproduce.org" "/Users/Matt/org/projects/sarah_forever.org" "/Users/Matt/org/projects/website.org" "/Users/Matt/Documents/Research/thesis/thesis/thesis.org") nil nil "Customized with use-package org")
 '(package-selected-packages
   '(org org-roam zotxt yasnippet-snippets yaml-mode writeroom-mode which-key unfill undo-tree spaceline request-deferred refine rainbow-delimiters quelpa-use-package poly-markdown pipenv persp-mode pdf-tools ox-hugo org-roam-bibtex org-ref org-journal org-inline-pdf org-fragtog org-bullets neotree multiple-cursors monokai-theme magit lsp-ui lsp-julia latex-extra jupyter ivy-bibtex hl-todo helm-lsp god-mode general fullframe eyebrowse elpy eglot-jl dimmer diminish counsel-projectile company-quickhelp company-org-roam company-auctex company-anaconda biblio auto-compile auctex-latexmk))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(safe-local-variable-values
   '((bibtex-completion-bibliography "./thesis.bib" "./me.bib")
     (bibtex-completion-bibliography quote
                                     ("./thesis.bib" "./me.bib"))
     (org-babel-default-header-args:julia
      (:session . "jl")
      (:kernel . "juliaorgroam-1.8")
      (:exports . "both"))
     (org-babel-default-header-args:jupyter-julia
      (:session . "jl")
      (:kernel . "juliaorgroam-1.8")
      (:exports . "both"))
     (org-babel-default-header-args:jupyter-julia quote
                                                  ((:session . "jl")
                                                   (:kernel . "juliaorgroam-1.8")
                                                   (:exports . "both")))
     (org-babel-default-header-args:jupyter-julia
      '((:session . "jl")
        (:kernel . "juliaorgroam-1.8")
        (:exports . "both")))
     (org-confirm-babel-evaluate)
     (org-babel-default-header-args:julia quote
                                          ((:session . "jl")
                                           (:kernel . "juliaorgroam-1.8")
                                           (:exports . "both")))
     (org-confirm-babel-evaluate nil)
     (org-babel-default-header-args:julia
      '((:session . "jl")
        (:kernel . "juliaorgroam-1.8")
        (:exports . "both")))
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
     (org-notes-root-file . \.\./math\.org)
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
 '(org-roam-link ((t (:inherit org-link :foreground "#9c8321")))))
