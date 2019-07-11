


(defvar mattroot/org-macros-funcs
  (list
   '("if-defined-ref-else-link" . "(eval
				   (if (member $1 (org-ref-get-custom-ids))
				       (concat \"\[\[ref:\" $1 \"\]\]\")
				     $2))"))
  "Functional org macros to be used globally")

(provide 'func-macros)



;; (if (member "ref-org" '("this" "that" "ref-org"))
;;     (concat "ref:" "ref-org")
;;   "nothing")


