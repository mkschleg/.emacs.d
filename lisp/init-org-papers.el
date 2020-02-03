

(require 'zotxt)
(require 'org-dir-tree)

(defgroup org-papers nil
  "Options in relation to org-papers in emacs"
  :tag "Org Papers"
  :group 'org)

(defconst org-papers--zotero-id-property "ZOTERO_ID"
  "Property for zotero id")
(defconst org-papers--notes-id-property "NOTEFILE"
  "Property for notes file")


(defcustom org-papers-initial-display-level 1
  "Set the initial display level for the display tree view"
  :group 'org-papers
  :type 'integer)

(defcustom org-papers-tab "  "
  "Tab characters used for indenting the dir view of note."
  :group 'org-papers
  :type 'string)

(defvar org-papers--paper-note-setupfile "/Users/Matt/Documents/Research/notes/setup/paper.org"
  "Setup file for notes on papers.")
(defvar org-papers--root-file "papers.org"
  "Name of root file for papers.")
(defvar org-papers--note-root-directory "/Users/Matt/Documents/Research/notes/papers/"
  "Root directory of paper notes.")

(defconst org-papers-buffer-name "*org papers*"
  "Buffer name of dired view.")

(setq org-papers--expanded-headings ())

(defun org-papers--insert-zotero-id (item)
  (insert (plist-get item :key)))

(defun org-papers--set-notes-property (id &optional folder arg)
  (unless folder (setq folder ""))
  (lexical-let ((item-id id)
		(folder folder)
		(arg arg))
    (deferred:$
      (request-deferred
       (format "%s/items" zotxt-url-base)
       :params `(("key" . ,item-id) ("format" . "paths"))
       :parser 'json-read)
      (deferred:nextc it
	(lambda (response)
	  (let ((paths (cdr (assq 'paths (elt (request-response-data response) 0)))))
	    (org-set-property
	     org-papers--notes-id-property
	     (concat folder
		     (mattroot/org-zotxt-org-file (org-zotxt-choose-path paths)))))))
      (if zotxt--debug-sync (deferred:sync! it)))))

(defun org-papers--insert-paper-heading-from-item (item)
  ;; (mapcar
  ;;  (lambda (key)
  ;;    (insert (symbol-name key)))
  ;;  (plist-get-keys item)))
  (org-insert-heading-after-current)
  (insert (plist-get item :matt-short))
  (org-set-property org-papers--zotero-id-property (plist-get item :key))
  (org-papers--set-notes-property
   (plist-get item :key)
   (mapconcat 
    (lambda (itm)
      (file-name-as-directory
       (downcase (replace-regexp-in-string " " "_" itm))))
    (org-get-outline-path)
    "")))

;; :key:citation:citation-html:matt-short:matt-short-html

(defun org-papers--insert-paper-heading (&optional arg)
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
            (org-papers--insert-paper-heading-from-item (car items)))))
      (deferred:error it
        (lambda (err)
          (error (error-message-string err))))
      (if zotxt--debug-sync (deferred:sync! it)))))

(defun org-papers--open-zotero-attachment-from-id (id &optional arg)
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
            (org-open-file (org-zotxt-choose-path paths) arg))))
      (if zotxt--debug-sync (deferred:sync! it)))))

(defun org-papers--open-zotero-attachment (&optional arg)
  (interactive "P")
  (org-papers--open-zotero-attachment-from-id
   (org-entry-get (point) org-papers--zotero-id-property)
   arg))

(defun org-papers--create-paper-note-file (path title)
  (if (not (file-exists-p path))
      (with-temp-file path
	(insert "#+title: " title "\n")
	(insert "#+setupfile: " (file-relative-name org-papers--paper-note-setupfile (file-name-directory path)) "\n")
	(insert "\n\n\n"))))

(defun org-papers--open-notes-file (&optional notes-file)
  (interactive)
  (unless notes-file (org-entry-get (point) org-papers--notes-id-property))
  (let* ((filename notes-file)
	 (bfr (find-buffer-visiting filename)))
    (if (not (file-exists-p filename))
	(org-papers--create-paper-note-file filename (org-entry-get (point) "ITEM")))
    (if bfr
	(set-window-buffer (selected-window) bfr)
      (find-file-existing filename))))


(defun org-papers--take-notes (zotero-id notes-file)
  (org-papers--open-notes-file notes-file)
  (org-papers--open-zotero-attachment-from-id zotero-id))

(defun org-papers-take-notes ()
  (interactive)
  (let ((notes-file (org-entry-get nil org-papers--notes-id-property))
	(zotero-id (org-entry-get nil org-papers--zotero-id-property)))
    ;; (find-file-existing notes-file)
    (org-papers--open-notes-file notes-file)
    (org-papers--open-zotero-attachment-from-id zotero-id)))

(defun org-papers-open-root-file ()
  (interactive)
  (find-file-existing (concat org-papers--note-root-directory org-papers--root-file)))


;;;;;;;;;;
;;
;; Interactions
;;
;;;;;;;;;;;

(defun org-papers--toggle-heading-at-point-for-expand (&optional POS)
  (unless POS (setq POS (point)))
  (let ((cur-lbl (button-label (button-at POS))))
    (if (not org-papers--expanded-headings)
	(setq org-papers--expanded-headings (list cur-lbl))
	(if (member cur-lbl org-papers--expanded-headings)
	    (setq org-papers--expanded-headings (remove cur-lbl org-papers--expanded-headings))
	  (add-to-list 'org-papers--expanded-headings cur-lbl)))))

(defun org-papers--next-pos ()
  (interactive)
  (if (< org-papers--cur-pos-idx org-papers--num-pos)
      (progn
	(setq org-papers--cur-pos-idx (+ org-papers--cur-pos-idx 1))
	(goto-char (nth (- (- org-papers--num-pos 1) org-papers--cur-pos-idx) org-papers--cur-pos-list)))))

(defun org-papers--prev-pos ()
  (interactive)
  (if (< 0 org-papers--cur-pos-idx)
      (progn
	(setq org-papers--cur-pos-idx (- org-papers--cur-pos-idx 1))
	(goto-char (nth (- (- org-papers--num-pos 1) org-papers--cur-pos-idx) org-papers--cur-pos-list)))))

(defun org-papers--follow-link-in-window (link &optional window to_quit)
  (unless window (setq window org-papers/file-window))
  (setq org-papers/view-window (selected-window))
  (select-window window)
  (let ((bfr (find-buffer-visiting link)))
    (if bfr
	(set-window-buffer window bfr)
      (find-file-existing link)))
  (if to_quit
      (kill-buffer org-papers-buffer-name)
    (select-window org-papers/view-window)))


;;;;;;;;;;
;;
;; Visualizations
;;
;;;;;;;;;;



(defun org-papers--add-cur-pos-to-list (&optional POS)
  (unless POS (setq POS (point)))
  (push (point) org-papers--cur-pos-list)
  (setq org-papers--num-pos (+ org-papers--num-pos 1)))

(defun org-papers--set-pos-idx (idx)
  (setq-local org-papers--cur-pos-idx idx)
  (goto-char (nth (- (- org-papers--num-pos 1) org-papers--cur-pos-idx) org-papers--cur-pos-list)))

(defun org-papers--set-cur-node (node)
  (setq-local org-papers--current-notes-node node)
  (org-papers-redisplay-from-node node))

(defun org-papers--refresh-buffer ()
  (erase-buffer)
  (setq-local org-papers--cur-pos-list ())
  (setq-local org-papers--num-pos 0))

(defun org-papers--insert-redraw-button (node)
  (if node
      (insert-button
       (org-dir-tree/node-name node)
       'action
       (lambda (btn)
	 (interactive)
	 (org-papers--set-cur-node (button-get btn 'node))
	 (org-papers--set-pos-idx 0))
       'node
       node)))

(defun org-papers--insert-button-dired (tree &optional TAB LEVEL)
  (unless TAB (setq TAB ""))
  (if tree ;;(and tree (or (not LEVEL) (<= LEVEL 0)))
      (progn
	(if (or (not LEVEL)
		(> LEVEL 0))
	    (progn
	      (insert TAB)
	      (org-papers--add-cur-pos-to-list)
	      (org-papers--insert-redraw-button (car tree))
	      (insert "\n")))
	(org-papers--insert-button-dired
	 (org-dir-tree/node-children (car tree))
	 (concat TAB org-papers-tab)
	 (if (member (org-dir-tree/node-name (car tree)) org-papers--expanded-headings)
	     LEVEL
	   (if LEVEL
	       (- LEVEL 1))))
	(org-papers--insert-button-dired (cdr tree) TAB LEVEL))))

(defun org-papers--insert-dired (cur-node parent-node)
  (if parent-node
      (progn
	(insert "Parent: [")
	(org-papers--add-cur-pos-to-list)
	(org-papers--insert-redraw-button parent-node)
	(insert "]\n")))
  (insert "Current: [")
  (org-papers--add-cur-pos-to-list)
  (org-papers--insert-redraw-button
   cur-node)
  (insert "]\n\n")
  (org-papers--insert-button-dired
   (org-dir-tree/node-children cur-node)
   "   "
   org-papers-initial-display-level))

(defun org-papers--display-header ()
  "Notes tree hierarchy: \n\n"
  )


(defun org-papers--redraw-buffer (cur-node cur-parent)
  (let ((inhibit-read-only t))
    (org-papers--refresh-buffer)
    (insert (org-papers--display-header))
    (org-papers--insert-dired cur-node cur-parent)))


(defun org-papers-redisplay-from-node (node)
  (interactive)
  (let ((cur-parent (org-dir-tree/find-parent-node-of-node node org-papers--current-tree))
	(cursor-idx org-papers--cur-pos-idx))
    (org-papers--redraw-buffer node cur-parent)
    (org-papers--set-pos-idx cursor-idx)))


(setq org-papers--current-notes-file nil)

(defun org-papers-visualize (&optional FILENAME)
  (interactive)
  (unless FILENAME (setq FILENAME (buffer-file-name)))
  (setq org-papers/file-window (selected-window))

  (if (not (and
	    org-papers--current-notes-file
	    (file-equal-p FILENAME org-papers--current-notes-file)))
      (progn
	(setq-local org-papers--expanded-headings ())
	(setq-local org-papers--current-notes-file FILENAME)))
  (setq org-papers--current-tree (org-dir-tree-get-tree (org-dir-tree--get-root-file FILENAME)))
  (let ((cur-parent (org-dir-tree/find-node-file (org-dir-tree--get-parent-file FILENAME) org-papers--current-tree))
	(cur-node (org-dir-tree/find-node-file FILENAME org-papers--current-tree)))
    (setq org-papers--current-notes-node cur-node)
    (select-window
     (display-buffer-in-side-window
      (get-buffer-create org-papers-buffer-name) '((side . right))))
    (set-buffer org-papers-buffer-name)
    (org-papers-mode)
    (org-papers--redraw-buffer cur-node cur-parent)
    (org-papers--set-pos-idx 0)))

(defun org-papers-display-dired (&optional FILENAME)
  (interactive)
  (org-papers-visualize FILENAME org-papers/dired-mode))

(defun org-papers-display-tree (&optional FILENAME)
  (interactive)
  (org-papers-visualize FILENAME org-papers/tree-mode))


(defvar org-papers-mode-map nil "Keymap for `org-papers-mode'")

(progn
  (setq org-papers-mode-map (make-sparse-keymap))
  (define-key org-papers-mode-map (kbd "q") 'kill-current-buffer)
  (define-key org-papers-mode-map (kbd "n") 'org-papers--next-pos)
  (define-key org-papers-mode-map (kbd "p") 'org-papers--prev-pos)
  (define-key org-papers-mode-map (kbd "o") (lambda ()
					     (interactive)
					     (let* ((cur-btn (button-at (point)))
						    (props (org-dir-tree/node-props (button-get cur-btn 'node)))
						    (link (alist-get org-papers--notes-id-property props nil nil 'string=)))
					       (if link
						   (org-papers--follow-link-in-window link)))))
  (define-key org-papers-mode-map (kbd "m") (lambda ()
					     (interactive)
					     (setq POS (point))
					     (org-papers--toggle-heading-at-point-for-expand POS)
					     (org-papers-redisplay-from-node org-papers--current-notes-node)
					     (goto-char POS)))
  (define-key org-papers-mode-map (kbd "r") (lambda ()
					     (interactive)
					     (org-papers-redisplay-from-node org-papers--current-notes-node)))
  (define-key org-papers-mode-map (kbd "e") (lambda ()
					      (interactive)
					      (let* ((cur-btn (button-at (point)))
						     (props (org-dir-tree/node-props (button-get cur-btn 'node)))
						     (notes-file (alist-get org-papers--notes-id-property props nil nil 'string=))
						     (zotero-id (alist-get org-papers--zotero-id-property props nil nil 'string=)))
						(kill-buffer org-papers-buffer-name)
						(org-papers--take-notes zotero-id notes-file)))))


(define-derived-mode org-papers-mode special-mode "notes"
  "org-papers-mode is a major mode for the org-papers buffer.")


(root-leader
  "p" '(:ignore t :which-key "org-papers"))

(root-leader
  "po" 'org-papers-open-root-file
  "pp" 'org-papers--open-zotero-attachment
  "pn" 'org-papers--open-notes-file
  "pe" 'org-papers-take-notes
  "pa" 'org-papers--insert-paper-heading)


(provide 'init-org-papers)
