


;; Org-roam
;;   For flat org-note taking structure.

(setq org-roam-v2-ack t)

(root-leader
  "n" '(:ignore t :which-key "notes")
  "nr" 'org-ref-ivy-insert-cite-link)

(use-package org-roam
      ;; :hook 
      ;; (after-init . org-roam-mode)
      :custom
      (org-roam-directory "~/org/notes")
      ;; (org-roam-index-file "index.org")
      ;; (org-roam-link-title-format "%s")
      :custom-face
      (org-roam-link ((t (:inherit org-link :foreground "#9c8321"))))
      :bind (("M-m n l" . org-roam-buffer)
             ("M-m n f" . org-roam-node-find)
             ("M-m n i" . org-roam-node-insert)
             ("M-m n o" . org-roam-jump-to-index)
             ("M-m n e" . org-roam-ox-hugo-export-zettle))
      :config

      ;; Start org-roam sync
      (org-roam-db-autosync-mode)
      (require 'org-roam-protocol)

      (setq org-ref-notes-function 'orb-org-ref-edit-note
            org-ref-completion-library 'org-ref-ivy-bibtex)
      ;; find-file-wildcards
      ;; (setq org-id-extra-files (find-lisp-find-files org-roam-directory "\.org$"))

      ;; Capture templates
      (setq org-roam-capture-templates
        '(("d" "default" plain "%?" :target
           (file+head "${slug}.org"
                       "#+SETUPFILE:../org-support-files/hugo_setup.org
#+HUGO_SECTION: braindump
#+HUGO_SLUG: ${slug}
#+hugo_custom_front_matter: :notetype note
#+TITLE: ${title}
#+include: ../org-support-files/variables.org\n")
           :unnarrowed t)
          ("r" "ref" plain "%?" :target
           (file+head "%(mattroot/ob-paper-file-name \"${title}\" \"${citekey}\").org"
                     "#+SETUPFILE:../org-support-files/hugo_setup.org
#+HUGO_SECTION: braindump
#+HUGO_SLUG: ${citekey}
#+hugo_custom_front_matter: :notetype paper
#+include: ../org-support-files/variables.org
#+TITLE: ${citekey}: ${title}

- tags :: \n- source :: ${url}\n- authors :: ${author-or-editor}\n- year :: ${year}


* References
bibliographystyle:author-year
bibliography:/Users/Matt/org/bib/full_library.bib
")
           :unnarrowed t)))

      (defun mattroot/org-roam--backlinks-list-with-content (file)
        (with-temp-buffer
          (if-let* ((backlinks (org-roam--get-backlinks file))
                    (grouped-backlinks (--group-by (nth 0 it) backlinks)))
              (progn
                (insert (format "\n\n* %d Backlinks\n"
                                (length backlinks)))
                (dolist (group grouped-backlinks)
                  (let ((file-from (car group))
                        (bls (cdr group)))
                    (insert (format "** [[file:%s][%s]]\n"
                                    file-from
                                    (org-roam-db--get-title file-from)))
                    (dolist (backlink bls)
                      (pcase-let ((`(,file-from _ ,props) backlink))
                        (insert (s-trim (s-replace "\n" " " (if (plist-get props :content)
                                                                (plist-get props :content)
                                                              ""))))
                        (insert "\n\n")))))))
          (buffer-string)))
      
      (defun mattroot/org-export-preprocessor (backend)
        ;; (let ((links (mattroot/org-roam--backlinks-list-with-content (buffer-file-name))))
        ;;   (unless (string= links "")
        ;;     (save-excursion
        ;;       (goto-char (point-max))
        ;;       (insert (concat "\n* Backlinks\n") links))))
        )

      (defun mattroot/org-id-update-org-roam-files ()
        "Update Org-ID locations for all Org-roam files."
        (interactive)
        (org-id-update-id-locations (org-roam--list-files org-roam-directory)))

      (add-hook 'org-export-before-processing-hook 'mattroot/org-export-preprocessor)

      (defun mattroot/force-org-rebuild-cache ()
        "Rebuild the `org-mode' and `org-roam' cache."
        (interactive)
        (org-id-update-id-locations)
        ;; Note: you may need `org-roam-db-clear-all'
        ;; followed by `org-roam-db-sync'
        (org-roam-db-sync)
        (org-roam-update-org-id-locations))

      )

(defun mattroot/ob-paper-file-name (title citekey)
  (downcase (replace-regexp-in-string "[,?.:;]" "" (s-replace-all '((" " . "_")) (concat citekey " " title)))))

(use-package org-roam-bibtex
  :ensure t
  :after org-roam
  :hook (org-mode . org-roam-bibtex-mode)
  :bind (("M-m n p" . orb-note-actions))
  :config
  (require 'org-ref)
  (setq orb-preformat-keywords
        '("citekey" "title" "url" "author-or-editor" "keywords" "file" "year")
        orb-process-file-keyword t
        orb-file-field-extensions '("pdf")
        orb-note-actions-interface 'ivy
        orb-insert-interface 'ivy-bibtex)


  (add-hook 'org-export-before-processing-hook 'mattroot/ob-export-preprocessor)


  (require 'zotxt)
  (require 'deferred)
  (require 'request)
  (require 'f)

  (defun org-roam--open-zotero-attachment-from-id (id &optional arg)
    (lexical-let ((item-id id)
                  (arg arg))
      (message item-id)
      (deferred:$
        (request-deferred
         (format "%s/items" zotxt-url-base)
         :params `(("key" . ,item-id) ("format" . "paths"))
         :parser 'json-read)
        (deferred:nextc it
          (lambda (response)
            (let ((paths (cdr (assq 'paths (elt (request-response-data response) 0)))))
              (find-file-other-window (org-zotxt-choose-path paths) arg))))
        (if zotxt--debug-sync (deferred:sync! it)))))

  (defun org-roam-bibtex-open-zotero-pdf ()
    (interactive)
    (org-roam--open-zotero-attachment-from-id (mattroot/org-kwd "ZOTERO")))

  (defun org-roam-ox-hugo-export-zettle ()
    (interactive)
    (mapc
     (lambda (file)
       (set-buffer (find-file-noselect file))
       (ignore-errors (org-hugo-export-wim-to-md)))
     (directory-files-recursively org-roam-directory "\\.org$"))))



;; (defun mattroot/ob-export-preprocessor (&optional backend)
;;     "Format citations and bibliography for BACKEND.
;; Warning.  Destructive to your document! Will replace links.
;; Meant to be used in export on a temporary version of the
;; documents."

;;     ;; Get the style from bibliographystyle link
;;     ;; and eliminate bibliography style links
;;     ;; This will load all style modules
;;     (cl-loop for link in (org-element-map
;;                              (org-element-parse-buffer) 'link 'identity)
;;              if (string= "bibliographystyle"
;;                          (org-element-property :type link))
;;              do
;;              ;; get path for style and load it
;;              (load-library (org-element-property :path link))
;;              ;; get rid of the link in the buffer
;;              (setf (buffer-substring (org-element-property :begin link)
;;                                      (org-element-property :end link))
;;                    ""))

;;     (orcp-collect-citations)
;;     (orcp-collect-unique-entries)

;;     (let ((link-replacements (cl-loop for link in *orcp-citation-links*
;;                                       for repl in (orcp-get-citation-replacements)
;;                                       collect
;;                                       (list repl
;;                                             (org-element-property :begin link)
;;                                             (org-element-property :end link)
;;                                             (org-element-property :path link))))
;;           (bibliography-string (orcp-formatted-bibliography))
;;           punctuation
;;           trailing-space
;;           bibliography-link)

;;       ;; replace citation links
;;       (cl-loop for (repl start end ref) in (reverse link-replacements)
;;                for link in (reverse *orcp-citation-links*)
;;                do
;;                ;; chomp leading spaces if needed
;;                (when (orcp-get-citation-style
;;                       'chomp-leading-space
;;                       (intern (org-element-property :type link)))
;;                  (goto-char start)
;;                  (while (and (not (sentence-beginning-p))
;;                              (looking-back " " (- (point) 2)))
;;                    (delete-char -1)
;;                    (setq start (- start 1))
;;                    (setq end (- end 1))))

;;                ;; chomp trailing spaces if needed
;;                (when (orcp-get-citation-style
;;                       'chomp-trailing-space
;;                       (intern (org-element-property :type link)))
;;                  (goto-char end)
;;                  (while (looking-back " " (- (point) 2))
;;                    (delete-char 1)))

;;                ;; Check for transposing punctuation
;;                (setq punctuation nil)
;;                (when (orcp-get-citation-style
;;                       'transpose-punctuation
;;                       (intern (org-element-property :type link)))
;;                  ;; goto end of link
;;                  (goto-char end)
;;                  (when (looking-at "\\.\\|,\\|;")
;;                    (setq punctuation (buffer-substring end (+ 1 end)))
;;                    (delete-char 1)))

;;                ;; preserve trailing space
;;                (goto-char end)
;;                (setq trailing-space (if (looking-back " " (line-beginning-position)) " " ""))

;;                (let* ((completions (org-roam--get-ref-path-completions))
;;                       (pair (assoc ref completions))
;;                       (file (if pair
;;                                 (cdr pair)
;;                               nil))
;;                       (link (mattroot/ob-get-ref-link-insert (cadr file) repl)))
;;                  (setf (buffer-substring start end) (concat link trailing-space)))


;;                (when punctuation
;;                  (goto-char start)
;;                  ;; I can't figure out why this is necessary. I would have thought
;;                  ;; the chomp leading spaces would get it.
;;                  (when (thing-at-point 'whitespace)
;;                    (delete-char -1))
;;                  (insert punctuation)))

;;       ;; Insert bibliography section at the bibliography link
;;       (setq bibliography-link (cl-loop for link
;;                                        in (org-element-map
;;                                               (org-element-parse-buffer)
;;                                               'link 'identity)
;;                                        if (string= "bibliography"
;;                                                    (org-element-property :type link))
;;                                        collect link))
;;       (pcase (length bibliography-link)
;;         ((pred (< 1)) (error "Only one bibliography link allowed"))
;;         ((pred (= 1))
;;          ;; replace bibliography link
;;          (setq bibliography-link (car bibliography-link))
;;          (setf (buffer-substring (org-element-property :begin bibliography-link)
;;                                  (oerg-element-property :end bibliography-link))
;;                bibliography-string))
;;         ((pred (= 0))
;;          ;; no bibliography link in document
;;          (when link-replacements
;;            (message "Warning: No bibliography link found although there are citations to process")))))



  ;; (setq orb-preformat-keywords
  ;;       '(("citekey" . "=key=") "title" "url" "file" "author-or-editor" "keywords" "year"))
  
;; ;;   (setq orb-templates
;; ;;       '(("r" "ref" plain (function org-roam-capture--get-point) "%?"
;; ;;          :file-name "%(mattroot/ob-paper-file-name \"${title}\" \"${citekey}\")"
;; ;;          :head "#+SETUPFILE:../hugo_setup.org
;; ;; #+HUGO_SECTION: braindump
;; ;; #+HUGO_SLUG: ${slug}
;; ;; #+hugo_custom_front_matter: :notetype paper
;; ;; #+ZOTERO: 
;; ;; #+TITLE: ${citekey}: ${title}
;; ;; #+include: ../../org-support-files/variables.org
;; ;; #+ROAM_KEY: ${ref}\n\n- tags :: \n- source :: ${url}\n- authors :: ${author-or-editor}\n- year :: ${year}



;; ;; * References
;; ;; bibliographystyle:author-year
;; ;; bibliography:/Users/Matt/org/bib/full_library.bib
;; ;; "
;; ;;          :unnarrowed t)))

;;   ;; (require 'org-ref-citeproc)

;;   (add-hook 'org-export-before-processing-hook 'mattroot/ob-export-preprocessor)


;;   (require 'zotxt)
;;   (require 'deferred)
;;   (require 'request)
;;   (require 'f)

;;   (defun org-roam--open-zotero-attachment-from-id (id &optional arg)
;;     (lexical-let ((item-id id)
;;                   (arg arg))
;;       (message item-id)
;;       (deferred:$
;;         (request-deferred
;;          (format "%s/items" zotxt-url-base)
;;          :params `(("key" . ,item-id) ("format" . "paths"))
;;          :parser 'json-read)
;;         (deferred:nextc it
;;           (lambda (response)
;;             (let ((paths (cdr (assq 'paths (elt (request-response-data response) 0)))))
;;               (find-file-other-window (org-zotxt-choose-path paths) arg))))
;;         (if zotxt--debug-sync (deferred:sync! it)))))

;;   (defun org-roam-bibtex-open-zotero-pdf ()
;;     (interactive)
;;     (org-roam--open-zotero-attachment-from-id (mattroot/org-kwd "ZOTERO")))

;;   (defun org-roam-ox-hugo-export-zettle ()
;;     (interactive)
;;     (mapc
;;      (lambda (file)
;;        (set-buffer (find-file-noselect file))
;;        (ignore-errors (org-hugo-export-wim-to-md)))
;;      (directory-files-recursively org-roam-directory "\\.org$"))))



;; (defun mattroot/ob-export-preprocessor (&optional backend)
;;     "Format citations and bibliography for BACKEND.
;; Warning.  Destructive to your document! Will replace links.
;; Meant to be used in export on a temporary version of the
;; documents."

;;     ;; Get the style from bibliographystyle link
;;     ;; and eliminate bibliography style links
;;     ;; This will load all style modules
;;     (cl-loop for link in (org-element-map
;;                              (org-element-parse-buffer) 'link 'identity)
;;              if (string= "bibliographystyle"
;;                          (org-element-property :type link))
;;              do
;;              ;; get path for style and load it
;;              (load-library (org-element-property :path link))
;;              ;; get rid of the link in the buffer
;;              (setf (buffer-substring (org-element-property :begin link)
;;                                      (org-element-property :end link))
;;                    ""))

;;     (orcp-collect-citations)
;;     (orcp-collect-unique-entries)

;;     (let ((link-replacements (cl-loop for link in *orcp-citation-links*
;;                                       for repl in (orcp-get-citation-replacements)
;;                                       collect
;;                                       (list repl
;;                                             (org-element-property :begin link)
;;                                             (org-element-property :end link)
;;                                             (org-element-property :path link))))
;;           (bibliography-string (orcp-formatted-bibliography))
;;           punctuation
;;           trailing-space
;;           bibliography-link)

;;       ;; replace citation links
;;       (cl-loop for (repl start end ref) in (reverse link-replacements)
;;                for link in (reverse *orcp-citation-links*)
;;                do
;;                ;; chomp leading spaces if needed
;;                (when (orcp-get-citation-style
;;                       'chomp-leading-space
;;                       (intern (org-element-property :type link)))
;;                  (goto-char start)
;;                  (while (and (not (sentence-beginning-p))
;;                              (looking-back " " (- (point) 2)))
;;                    (delete-char -1)
;;                    (setq start (- start 1))
;;                    (setq end (- end 1))))

;;                ;; chomp trailing spaces if needed
;;                (when (orcp-get-citation-style
;;                       'chomp-trailing-space
;;                       (intern (org-element-property :type link)))
;;                  (goto-char end)
;;                  (while (looking-back " " (- (point) 2))
;;                    (delete-char 1)))

;;                ;; Check for transposing punctuation
;;                (setq punctuation nil)
;;                (when (orcp-get-citation-style
;;                       'transpose-punctuation
;;                       (intern (org-element-property :type link)))
;;                  ;; goto end of link
;;                  (goto-char end)
;;                  (when (looking-at "\\.\\|,\\|;")
;;                    (setq punctuation (buffer-substring end (+ 1 end)))
;;                    (delete-char 1)))

;;                ;; preserve trailing space
;;                (goto-char end)
;;                (setq trailing-space (if (looking-back " " (line-beginning-position)) " " ""))

;;                (let* ((completions (org-roam--get-ref-path-completions))
;;                       (pair (assoc ref completions))
;;                       (file (if pair
;;                                 (cdr pair)
;;                               nil))
;;                       (link (mattroot/ob-get-ref-link-insert (cadr file) repl)))
;;                  (setf (buffer-substring start end) (concat link trailing-space)))


;;                (when punctuation
;;                  (goto-char start)
;;                  ;; I can't figure out why this is necessary. I would have thought
;;                  ;; the chomp leading spaces would get it.
;;                  (when (thing-at-point 'whitespace)
;;                    (delete-char -1))
;;                  (insert punctuation)))

;;       ;; Insert bibliography section at the bibliography link
;;       (setq bibliography-link (cl-loop for link
;;                                        in (org-element-map
;;                                               (org-element-parse-buffer)
;;                                               'link 'identity)
;;                                        if (string= "bibliography"
;;                                                    (org-element-property :type link))
;;                                        collect link))
;;       (pcase (length bibliography-link)
;;         ((pred (< 1)) (error "Only one bibliography link allowed"))
;;         ((pred (= 1))
;;          ;; replace bibliography link
;;          (setq bibliography-link (car bibliography-link))
;;          (setf (buffer-substring (org-element-property :begin bibliography-link)
;;                                  (org-element-property :end bibliography-link))
;;                bibliography-string))
;;         ((pred (= 0))
;;          ;; no bibliography link in document
;;          (when link-replacements
;;            (message "Warning: No bibliography link found although there are citations to process"))))))

(defun mattroot/ob-export-preprocessor (&optional backend)
  )
  ;; (when (equal backend 'hugo)
  ;;   (when (org-roam--org-roam-file-p)
  ;;     (beginning-of-buffer)
  ;;     (replace-string "{" "")
  ;;     (beginning-of-buffer)
  ;;     (replace-string "}" "")
  ;;     (end-of-buffer)
  ;;     (org-roam-buffer--insert-backlinks))))


;; (with-current-buffer "*scratch*"
;;   (insert (string-join (directory-files-recursively org-roam-directory "\\.org$") "\n"))
;;   )
;; (org-hugo- "~/org/notes/anatomy.org")

;; (defun mattroot/ob-get-ref-link-insert (file repl)
;;   (if file
;;       (org-roam-format-link file repl)
;;     repl))

(defun mattroot/org-kwds ()
  "parse the buffer and return a cons list of (property . value)
from lines like:
#+PROPERTY: value"
  (org-element-map (org-element-parse-buffer 'element) 'keyword
                   (lambda (keyword) (cons (org-element-property :key keyword)
                                           (org-element-property :value keyword)))))

(defun mattroot/org-kwd (KEYWORD)
  "get the value of a KEYWORD in the form of #+KEYWORD: value"
  (cdr (assoc KEYWORD (mattroot/org-kwds))))


(defun org-notes--copy-paper-name-from-item (item)
  (kill-new (plist-get item :matt-short))
  )


(defun org-notes-get-paper-name (&optional arg)
    "Insert a zotero link in the `org-mode' document.

Prompts for search to choose item.  If prefix argument ARG is used,
will insert the currently selected item from Zotero.  If double
prefix argument is used the search method will have to be
selected even if `org-zotxt-default-search-method' is non-nil"
  (interactive "P")
  (lexical-let ((mk (point-marker)))
    (deferred:$
      (if (equal '(4) arg)
          (zotxt-get-selected-items-deferred)
        (zotxt-choose-deferred (unless (equal '(16) arg) org-zotxt-default-search-method)))
      (deferred:nextc it
        (lambda (items)
          (if (null items)
              (error "No item found for search")
            (zotxt-mapcar-deferred #'org-zotxt-get-item-link-text-deferred items))))
      (deferred:nextc it
        (lambda (items)
          (with-current-buffer (marker-buffer mk)
            (goto-char (marker-position mk))
            (org-notes--copy-paper-name-from-item (car items)))))
      (deferred:error it
        (lambda (err)
          (error (error-message-string err))))
      (if zotxt--debug-sync (deferred:sync! it)))))

(use-package company-org-roam
  :quelpa
  (company-org-roam :fetcher github :repo "jethrokuan/company-org-roam")
  :config
  (push 'company-org-roam company-backends))


(provide 'init-org-notes)
