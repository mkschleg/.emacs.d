

(use-package ox
  :straight nil
  :after org
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  )

(use-package ox-hugo
  :straight t
  :after ox
  :config
)
  ;; (require 'ox-extra)
  ;; (ox-extras-activate '(ignore-headlines)))

(use-package ox-latex
  :straight nil
  :after ox
  :config


  ;;;;;
  ;; Thesis Latex Config
  ;;;;;
  (add-to-list 'org-latex-classes
               '("thesis"
                 "
\\providecommand{\\main}{.}
\\documentclass[12pt]{report}          % for default format
\\input{thesis_header.tex}
[EXTRA]
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]
                  "
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\mboxparagraph{%s}" . "\\mboxparagraph*{%s}")
                 ("\\mboxsubparagraph{%s}" . "\\mboxsubparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("note"
                 "
\\documentclass[11pt]{article}          % for default format
\\usepackage[
backend=biber,
style=numeric,
sorting=ynt,
natbib=true
]{biblatex}
\\addbibresource{../bib/full_library.bib}
"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\mboxparagraph{%s}" . "\\mboxparagraph*{%s}")
                 ("\\mboxsubparagraph{%s}" . "\\mboxsubparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("statement"
                 "
\\documentclass[11pt]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}

\\usepackage[hidelinks]{hyperref}
\\usepackage{fullpage}
\\usepackage[
    backend=biber,
    style=numeric,
    url=false,
    isbn=false
]
{biblatex}
\\addbibresource{/Users/Matt/org/bib/full_library.bib}
\\usepackage{fancyhdr}
\\fancyhead[L]{Matthew Schlegel}
\\fancyhead[R]{\\today}
\\renewcommand{\\headrulewidth}{0.4pt}
\\setlength{\\headheight}{14pt}

\\pagestyle{fancy}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]
"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\mboxparagraph{%s}" . "\\mboxparagraph*{%s}")
                 ("\\mboxsubparagraph{%s}" . "\\mboxsubparagraph*{%s}")))
  )

(setq mattroot/temp-variable
      `("1"
        "2"
        "3"))


(provide 'init-org-export)
