


(add-to-list 'load-path (expand-file-name "lisp/org-macros" user-emacs-directory))

(require 'func-macros)
(require 'math-macros)


(defvar mattroot/org-macros
  (append
   mattroot/org-macros-funcs
   mattroot/org-macros-math
   )
  "Macros to be used globally")

(provide 'init-org-macros)
