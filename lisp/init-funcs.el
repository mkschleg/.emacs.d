


;; Given ("a", "b", "c"), return "1. a, 2. b, 3. c".

;; (defun mattroot/hydra-zip-int-string (list-a list-b)
;;   (mapconcat
;;      (lambda (a b)
;;        (format "_%d_ %s" (cl-incf i) x))
;;      list
;;      ", ")
;;   )



(defun mattroot/number-names (list)
  "Enumerate and concatenate LIST."
  (let ((i 0))
    (mapconcat
     (lambda (x)
       (format "_%d_ %s" (cl-incf i) x))
     list
     " ")))


(provide 'init-funcs)
