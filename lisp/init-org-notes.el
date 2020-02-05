

(require 'org)
(require 'org-dir-tree)
;; (require 'ido)

;; (defcustom )
(defgroup org-notes nil
  "Options in relation to org-notes in emacs"
  :tag "Org Notes"
  :group 'org)

(defcustom org-notes-tab "  "
  "Tab characters used for indenting the dir view of note."
  :group 'org-notes
  :type 'string)

(defcustom org-notes-follow-link-and-quit nil
  "If set to a value then quit org-notes mode when following a link"
  :group 'org-notes
  :type 'boolean)

(defcustom org-notes-initial-display-level 1
  "Set the initial display level for the display tree view"
  :group 'org-notes
  :type 'integer)

(defcustom org-notes-buffer-name "*org-notes-temp*"
  "Set the name of the temporary org-notes buffer."
  :group 'org-notes
  :type 'string)

(defcustom org-notes--index-setup-file "/Users/Matt/Documents/Research/notes/setup/index.org"
  "File for setup of index files."
  :group 'org-notes
  :type 'string)

(defcustom org-notes--file-setup-file "/Users/Matt/Documents/Research/notes/setup/nugget.org"
  "File for setup of final files files."
  :group 'org-notes
  :type 'string)



;; All bow to the holy tree
(setq org-notes--current-tree nil)

(setq org-notes--cur-node-for-tree nil)
(setq org-notes--current-notes-file nil)
(setq org-notes--current-notes-node nil)
(setq org-notes--current-expanded-child nil)
(setq org-notes--current-mode 1)

;; The window the orgmode file is being displayed in.
(setq org-notes/file-window nil)

;; Dired mode Vars
(setq org-notes--expanded-headings ())

;; For movement/selection
(setq org-notes--cur-pos-list ())
(setq org-notes--cur-pos-idx 0)
(setq org-notes--num-pos 0)


(defconst org-notes/root-dir-property "ROOT-DIR"
  "Property key for getting root dir from info org file.")
(defconst org-notes/root-file-property "ROOT-FILE"
  "Property key for getting root dir from info org file.")
(defconst org-notes/parent-file-property "PARENT-FILE"
  "Property key for getting root dir from info org file.")

(defconst org-notes/index-file-property "NOTE-INDEX"
  "Property key for relative path to the subindex file.")

(defconst org-notes/dired-mode 1
  "Value for dired like mode for note visualization.")
(defconst org-notes/tree-mode 2
  "Value for tree like mode for note visualization.")
;; (defconst org-notes/modes '(org-notes/dired-mode org-notes/tree-mode)
;;   "List containing all the modes used for org-notes. This is a simple state-machine.")
(defun org-notes/toggle-mode (mode)
  (if (= mode org-notes/dired-mode)
      org-notes/tree-mode
    org-notes/dired-mode))


;;;;;;;
;;
;; Structure of org files
;;
;;;;;;;

(cl-defstruct org-notes/node name file position children)

(defun org-notes/make-node (name file position children)
  "Helper for making an org-notes/node."
  (make-org-notes/node :name name :file file :position position :children children))

(defun org-notes--get-headings-of-file (FILE &optional ABSPATH)
  (if (file-exists-p FILE)
      (org-map-entries
       (lambda ()
	 (interactive)
	 (let ((path ABSPATH))
	   (list
	    (org-entry-get nil "ITEM")
	    (if (org-entry-get nil org-notes/index-file-property)
		(concat path (org-entry-get nil org-notes/index-file-property))
	      nil)
	    (org-outline-level)
	    FILE
	    (point))))
       nil
       (list FILE))))

(defun org-notes--structure-tree-help (struct level)
  (let ((cur-node-list '()))
    (while (and struct (<= level (caddar struct)))
      (let ((cur-l (car struct))
	    (new-struct (cdr struct)))
	(setq struct (cdr struct))
	(push
	 (org-notes/make-node
	  (car cur-l) ;; NAME
	  (if (cadr cur-l)
	      (cadr cur-l)
	    (cadddr cur-l)) ;; FILE
	  (if (cadr cur-l)
	      nil
	    (cadddr (cdr cur-l))) ;; POSITION
	  (if (and
	       struct
	       (< level (caddar struct)))
	      (let ((tmp-struct (org-notes--structure-tree-help struct (caddar struct))))
		(setq struct (car tmp-struct))
		;; (setq new-struct (car tmp-struct))
		(cadr tmp-struct))
	    (if (cadr cur-l)
		(org-notes--structure-get-children (cadr cur-l))))) ;; CHILDREN
	 cur-node-list)))
    (list struct (reverse cur-node-list))))

(defun org-notes--structure-get-children (FILENAME)
  (cadr (org-notes--structure-tree-help
	 (org-notes--get-headings-of-file
	  FILENAME
	  (file-name-directory FILENAME))
	 1)))

(defun org-notes--structure-tree (FILENAME)
  (list (org-notes/make-node
	 (mattroot/org-kwd-file FILENAME "TITLE")
	 FILENAME
	 nil
	 (cadr (org-notes--structure-tree-help
		(org-notes--get-headings-of-file
		 FILENAME
		 (file-name-directory FILENAME))
		1)))))

(defun org-notes-get-tree (&optional FILENAME)
  (unless FILENAME (setq FILENAME (buffer-file-name)))
  (if FILENAME
      (org-notes--structure-tree FILENAME)))


;;;;;;;;;;;;
;; SEARCHING
;;;;;;;;;;;;

(defun org-notes/find-node--helper (func tree)
  (if tree
      (let ((current-node (car tree)))
	(if (funcall func current-node)
	    current-node
	  (let ((node (org-notes/find-node--helper func (org-notes/node-children current-node))))
	    (if node
		node
	      (org-notes/find-node--helper func (cdr tree))))))))

(defun org-notes/find-node (comp &optional tree)
  (unless tree (setq-local tree org-notes--current-tree))
  (org-notes/find-node--helper
   comp
   tree))

(defun org-notes/find-in-children (func node)
  (let ((res nil))
    (dolist
	(n (org-notes/node-children node) res)
      (setq res (or res (funcall func n))))))

(defun org-notes/find-in-children-name (name node)
  (let ((name name))
    (org-notes/find-in-children
     (lambda (n)
       (string-equal name (org-notes/node-name n)))
     node)))

(defun org-notes/is-child (node child-node)
  (let ((res nil))
    (dolist
	(n (org-notes/node-children node) res)
      (setq res (or res (equal n child-node))))))

(defun org-notes/find-parent-node--helper (func tree)
  (if tree
      (let ((current-node (car tree)))
	(if (org-notes/find-in-children func current-node)
	    current-node
	  (let ((node (org-notes/find-parent-node--helper func (org-notes/node-children current-node))))
	    (if node
		node
	      (org-notes/find-parent-node--helper func (cdr tree))))))))

(defun org-notes/find-parent-node (func &optional root)
  (unless root (setq root org-notes--current-tree))
  (org-notes/find-parent-node--helper func root))

(defun org-notes/find-parent-node-of-name (name &optional root)
  (let ((name name))
    (org-notes/find-parent-node
     (lambda (node)
       (string-equal name (org-notes/node-name node)))
     root)))

(defun org-notes/find-parent-node-of-node (node &optional root)
  (let ((curnode node))
    (org-notes/find-parent-node
     (lambda (node)
       (equal curnode node))
     root)))

(defun org-notes/find-node-name (name &optional tree)
  (let ((name name))
    (org-notes/find-node
     (lambda (node)
       (string-equal name (org-notes/node-name node)))
     tree)))

(defun org-notes/find-node-file (file &optional tree)
  (if file
      (let ((file file))
	(org-notes/find-node
	 (lambda (node)
	   (file-equal-p file (org-notes/node-file node)))
	 tree))))

(defun org-notes/is-leaf (node)
  (if (org-notes/node-children node) nil t))


(defun org-notes--get-directory-info (DIR)
  (let ((info-file (concat DIR "INFO.org")))
    (if (file-exists-p info-file)
	(cadr (assoc "INFO" (org-map-entries
		       (lambda ()
			 (list (org-entry-get nil "ITEM") (org-entry-properties)))
		       nil
		       (list info-file))))
      nil)))

(defun org-notes--get-file-directory-info (FILENAME)
  ;; (unless FILENAME (setq FILENAME (buffer-file-name)))
  (org-notes--get-directory-info (file-name-directory FILENAME)))

(defun org-notes--get-root-file (&optional FILENAME)
  (unless FILENAME (setq FILENAME (buffer-file-name)))
  (let ((props (org-notes--get-file-directory-info FILENAME)))
    (if props
	(if (cdr (assoc org-notes/root-file-property props))
	    (cdr (assoc org-notes/root-file-property props))
	  FILENAME)
      (let ((prnt-file (org-notes--get-parent-file FILENAME)))
	(if prnt-file
	    (org-notes--get-root-file prnt-file)
	  FILENAME)))))

(defun org-notes--get-parent-file (&optional FILENAME)
  (unless FILENAME (setq FILENAME (buffer-file-name)))
  (setq-local prnt-file (mattroot/org-kwd-file FILENAME "PARENTFILE"))
  (if prnt-file
      ;; If defined in the org-mode file it will be a relative path!
      (concat (file-name-directory FILENAME) prnt-file)
    (let ((props (org-notes--get-file-directory-info FILENAME)))
      (if (cdr (assoc org-notes/parent-file-property props))
	  (concat
	   (file-name-directory (cdr (assoc "FILE" props)))
	   (cdr (assoc org-notes/parent-file-property props)))
	nil))))


(defun org-notes-get-link ()
  (org-entry-get nil org-notes/index-file-property))

(defun org-notes--follow-link-in-window (link &optional window avoid_quit)
  (unless window (setq window org-notes/file-window))
  (setq org-notes/view-window (selected-window))
  (select-window window)
  (let ((bfr (find-buffer-visiting link)))
    (if bfr
	(set-window-buffer window bfr)
      (find-file-existing link)))
  (if (and (not avoid_quit) org-notes-follow-link-and-quit)
      (kill-buffer org-notes-buffer-name)
    (select-window org-notes/view-window)))

(defun org-notes-follow-entry-link ()
  (interactive)
  (find-file-existing (org-notes-get-link)))

(defun mattroot/org-kwds ()
  "parse the buffer and return a cons list of (property . value)
from lines like:
#+PROPERTY: value"
  (org-element-map (org-element-parse-buffer 'element) 'keyword
                   (lambda (keyword) (cons (org-element-property :key keyword)
                                           (org-element-property :value keyword)))))


;;;;;;;;;;;;
;;
;; Data config
;;
;;;;;;;;;;;;

(defun org-notes--get-index-folder-name (obj)
  (if (org-notes/node-p obj)
      (replace-regexp-in-string " " "_" (downcase (org-notes/node-name obj)))
    (replace-regexp-in-string " " "_" (downcase obj))))

(defun org-notes--get-index-folder-path (node)
  (let ((filename (org-notes/node-file node)))
    (file-truename
     (concat
      (file-name-directory filename)
      (org-notes--get-index-folder-name node)
      "/"))))

;;;;;;;;;;;;
;;
;; Interactions
;;
;;;;;;;;;;;;

(defun org-notes--toggle-heading-at-point-for-expand (&optional POS)
  (unless POS (setq POS (point)))
  (let ((cur-lbl (button-label (button-at POS))))
    (if (not org-notes--expanded-headings)
	(setq org-notes--expanded-headings (list cur-lbl))
	(if (member cur-lbl org-notes--expanded-headings)
	    (setq org-notes--expanded-headings (remove cur-lbl org-notes--expanded-headings))
	  (add-to-list 'org-notes--expanded-headings cur-lbl)))))

(defun org-notes--toggle-tree-root-at-point (&optional POS)
  (unless POS (setq POS (point)))
  (let ((cur-lbl (button-label (button-at POS))))
    (setq org-notes--cur-node-for-tree (org-notes/find-node-name cur-lbl))))

(defun org-notes--toggle-expanded-child (&optional POS)
  (unless POS (setq POS (point)))
  ;; (let ((cur-lbl (button-label (button-at POS))))
  ;;   (setq org-notes--cur-node-for-tree (org-notes/find-node-name cur-lbl)))
  (let ((cur-node (org-notes/find-node-name (button-label (button-at POS)))))
    (if (org-notes/is-child org-notes--current-notes-node cur-node)
	(setq org-notes--current-expanded-child cur-node))))

(defun org-notes--next-pos ()
  (interactive)
  (if (< org-notes--cur-pos-idx org-notes--num-pos)
      (progn
	(setq org-notes--cur-pos-idx (+ org-notes--cur-pos-idx 1))
	(goto-char (nth (- (- org-notes--num-pos 1) org-notes--cur-pos-idx) org-notes--cur-pos-list)))))

(defun org-notes--prev-pos ()
  (interactive)
  (if (< 0 org-notes--cur-pos-idx)
      (progn
	(setq org-notes--cur-pos-idx (- org-notes--cur-pos-idx 1))
	(goto-char (nth (- (- org-notes--num-pos 1) org-notes--cur-pos-idx) org-notes--cur-pos-list)))))

(defun org-notes--set-pos-idx (idx)
  (setq org-notes--cur-pos-idx idx)
  (goto-char (nth (- (- org-notes--num-pos 1) org-notes--cur-pos-idx) org-notes--cur-pos-list)))

(defun org-notes--add-cur-pos-to-list (&optional POS)
  (unless POS (setq POS (point)))
  (push (point) org-notes--cur-pos-list)
  (setq org-notes--num-pos (+ org-notes--num-pos 1)))

(defun org-notes--set-cur-node (node)
  (setq org-notes--current-notes-node node)
  (org-notes-redisplay-from-node node))


(defun org-notes--create-sub-index (INDEX-NAME filepath index-folder-path parent-index-path)
  ;; (if (not (file-exists-p filepath))
  ;;     (with-temp-file filepath
  ;; 	(insert
  ;; 	 "#+title: " INDEX-NAME "\n"
  ;; 	 "#+setupfile: ../../setup/index.org" "\n"
  ;; 	 "#+parentfile: " parent-index-path "\n\n\n\n")))
  (org-notes--create-index-file INDEX-NAME filepath parent-index-path)
  (if (not (file-directory-p index-folder-path))
      (make-directory index-folder-path)))

(defun org-notes--create-index-file (NAME filepath parent-index-path)
  (if (not (file-exists-p filepath))
      (with-temp-file filepath
	(insert
	 "#+title: " NAME "\n"
	 "#+setupfile: " (file-relative-name (file-truename org-notes--index-setup-file) (file-truename (file-name-directory filepath))) "\n"
	 "#+parentfile: " parent-index-path "\n\n\n\n"))))

(defun org-notes--create-file (NAME filepath parent-index-path)
  (if (not (file-exists-p filepath))
      (with-temp-file filepath
	(insert
	 "#+title: " NAME "\n"
	 "#+setupfile: " (file-relative-name
			  (file-truename org-notes--file-setup-file) (file-truename (file-name-directory filepath))) "\n"
	 "#+parentfile: " parent-index-path "\n\n\n\n"))))


(defun org-notes--add-sub-index-to-node (node INDEX-NAME)
  ;; Make index file
  (if (not (org-notes/find-in-children-name INDEX-NAME node))
      (let ((parent-index-folder-path (org-notes--get-index-folder-path node))
	    (parent-index-file-path (file-truename (org-notes/node-file node)))
	    (folder-name (org-notes--get-index-folder-name INDEX-NAME))
	    (sub-index-file-path (concat
				  (org-notes--get-index-folder-path node)
				  (org-notes--get-index-folder-name INDEX-NAME)
				  ".org")))
	;; (list folder-name file-name index-folder-name)
	(progn
	  (org-notes--create-sub-index
	   INDEX-NAME
	   sub-index-file-path
	   (concat parent-index-folder-path folder-name)
	   (file-relative-name parent-index-file-path (file-name-directory sub-index-file-path)))
	  (let ((bfr (find-buffer-visiting parent-index-file-path)))
	    (with-current-buffer (if bfr bfr (find-file-existing parent-index-file-path))
	      (end-of-buffer)
	      ;; (search-backward-regexp "^* " nil t)
	      ;; (org-insert-heading-after-current)
	      (if (search-backward-regexp "^* " nil t)
		(org-insert-heading-after-current)
	      (insert "* "))
	      (insert INDEX-NAME)
	      (org-set-property
	       org-notes/index-file-property
	       (file-relative-name sub-index-file-path (file-name-directory parent-index-file-path)))
	      (search-backward-regexp "^* " nil t)))
	  (push
	   (org-notes/make-node INDEX-NAME sub-index-file-path (point) nil)
	   (if (org-notes/node-children node)
	     (cdr (last (org-notes/node-children node)))
	   (org-notes/node-children node)))))
	   ;; (cdr (last (org-notes/node-children node))))))
    (message (concat INDEX-NAME " is already a heading."))))


(defun org-notes--add-file-note-to-node (node NAME)
  (if (not (org-notes/find-in-children-name NAME node))
      (let ((parent-index-folder-path (org-notes--get-index-folder-path node))
	    (parent-index-file-path (file-truename (org-notes/node-file node)))
	    (new-file-path (concat
			    (org-notes--get-index-folder-path node)
			    (org-notes--get-index-folder-name NAME)
			    ".org")))
	(org-notes--create-file NAME
				new-file-path
				(file-relative-name parent-index-file-path
						    (file-name-directory new-file-path)))
	(let ((bfr (find-buffer-visiting parent-index-file-path)))
	  (with-current-buffer (if bfr bfr (find-file-existing parent-index-file-path))
	    (end-of-buffer)
	    (if (search-backward-regexp "^* " nil t)
		(org-insert-heading-after-current)
	      (insert "* "))
	    (insert NAME)
	    (org-set-property
	     org-notes/index-file-property
	     (file-relative-name new-file-path (file-name-directory parent-index-file-path)))
	    (search-backward-regexp "^* " nil t)))
	(push
	 (org-notes/make-node NAME new-file-path (point) nil)
	 (if (org-notes/node-children node)
	     (cdr (last (org-notes/node-children node)))
	   (org-notes/node-children node))))
    (message (concat NAME " is already a heading."))))


(defun org-notes--add-leaf-note-to-node (node NAME)
    (if (not (org-notes/find-in-children-name NAME node))
      (let ((parent-index-folder-path (org-notes--get-index-folder-path node))
	    (parent-index-file-path (file-truename (org-notes/node-file node))))
	(let ((bfr (find-buffer-visiting parent-index-file-path)))
	  (with-current-buffer (if bfr bfr (find-file-existing parent-index-file-path))
	    (end-of-buffer)
	    (if (search-backward-regexp "^* " nil t)
		(org-insert-heading-after-current)
	      (insert "* "))
	    (insert NAME)
	    (search-backward-regexp "^* " nil t)))
	(push
	 (org-notes/make-node NAME (org-notes/node-file node) (point) nil)
	 (if (org-notes/node-children node)
	     (cdr (last (org-notes/node-children node)))
	   (org-notes/node-children node))))
      (message (concat NAME " is already a heading."))))

(progn
  (setq org-notes/add-node-hash (make-hash-table :test 'equal))
  (puthash "index" 'org-notes--add-sub-index-to-node org-notes/add-node-hash)
  (puthash "file" 'org-notes--add-file-note-to-node org-notes/add-node-hash)
  (puthash "leaf" 'org-notes--add-leaf-note-to-node org-notes/add-node-hash))

(defun org-notes--make-add-node-selection-list ()
  (hash-table-keys org-notes/add-node-hash))


(defun org-notes--open-heading-to-edit ()
  (interactive)
  (let ((btn-node (org-notes/find-node-name (button-label (button-at (point))))))
    (org-notes--follow-link-in-window
     (org-notes/node-file btn-node) nil t)
    (select-window org-notes/file-window)
    (if (org-notes/node-position btn-node)
	(goto-char (org-notes/node-position btn-node))
      (progn
	(goto-char 1)
	(search-forward-regexp "^* " nil t)))
    (outline-show-children)))


;;;;;;;;;;;;
;;
;; Visualizations
;;
;;;;;;;;;;;;

(defun org-notes--refresh-buffer ()
  (erase-buffer)
  (setq org-notes--cur-pos-list ())
  (setq org-notes--num-pos 0))

(defun org-notes--format-tree (tree &optional TAB)
  (unless TAB (setq-local TAB ""))
  (if tree
      (concat TAB "_" (org-notes/node-name (car tree)) "_" "\n"
	      (org-notes--format-tree (org-notes/node-children (car tree)) (concat TAB org-notes-tab))
	      (org-notes--format-tree (cdr tree) TAB))
    ""))

(defun org-notes--insert-button-with-link (node)
  (insert-button
   name
   'action
   (lambda (btn)
     (message (format "Button Pressed %s"
		      (concat (button-label btn) " "
			      (org-notes/node-file (org-notes/find-node-name (button-label btn))))))
     (org-notes--follow-link-in-window
      (org-notes/node-file (org-notes/find-node-name (button-get btn 'link)))))
   'node
   node
   'link
   (org-notes/node-file node)))

(defun org-notes--insert-redraw-button (node)
  (if node
      (insert-button
       (org-notes/node-name node)
       'action
       (lambda (btn)
	 (interactive)
	 (org-notes--set-cur-node (button-get btn 'node))
	 (org-notes--set-pos-idx 0))
       'node
       node)))

(defun org-notes--insert-button-dired (tree &optional TAB LEVEL)
  (unless TAB (setq-local TAB ""))
  (if tree ;;(and tree (or (not LEVEL) (<= LEVEL 0)))
      (progn
	(if (or (not LEVEL)
		(> LEVEL 0))
	    (progn
	      (insert TAB)
	      (org-notes--add-cur-pos-to-list)
	      (org-notes--insert-redraw-button (car tree))
	      (insert "\n")))
	(org-notes--insert-button-dired
	 (org-notes/node-children (car tree))
	 (concat TAB org-notes-tab)
	 (if (member (org-notes/node-name (car tree)) org-notes--expanded-headings)
	     LEVEL
	   (if LEVEL
	       (- LEVEL 1))))
	(org-notes--insert-button-dired (cdr tree) TAB LEVEL))))

(defun org-notes--insert-dired (cur-node parent-node)
  (if parent-node
      (progn
	(insert "Parent: [")
	(org-notes--add-cur-pos-to-list)
	(org-notes--insert-redraw-button parent-node)
	(insert "]\n")))
  (insert "Current: [")
  (org-notes--add-cur-pos-to-list)
  (org-notes--insert-redraw-button
   cur-node)
  (insert "]\n\n")
  (org-notes--insert-button-dired
   (org-notes/node-children cur-node)
   "   "
   org-notes-initial-display-level))

(defun org-notes--display-header ()
  "Notes tree hierarchy: \n\n"
  )

(defun org-notes--insert-tree (current-node parent-node)

  (if parent-node
      (progn
	(insert "Parent: [")
	(org-notes--add-cur-pos-to-list)
	(org-notes--insert-redraw-button parent-node)
	(insert "]\n\n")))

  (insert org-notes-tab)
  (org-notes--add-cur-pos-to-list)
  (org-notes--insert-redraw-button current-node)
  (if (not (org-notes/is-leaf current-node))
      (let ((nodes (org-notes/node-children current-node)))
	(if (equal (car nodes) org-notes--current-expanded-child)
	    (setq nodes (cdr nodes)))
	(insert "-->")
	(org-notes--add-cur-pos-to-list)
	(org-notes--insert-redraw-button (car nodes))
	(insert "\n")
	(let ((beg-str (make-string (length (org-notes/node-name current-node)) ?\s)))
	  (mapcar
	   (lambda (node)
	     (if (not (equal node org-notes--current-expanded-child))
		 (progn
		   (insert org-notes-tab)
		   (insert beg-str)
		   (insert "|->")
		   (org-notes--add-cur-pos-to-list)
		   (org-notes--insert-redraw-button node)
		   (insert "\n"))))
	   (cdr nodes))
	  (if org-notes--current-expanded-child
	      (progn
		(insert org-notes-tab)
		(insert beg-str)
		(insert "|\n")
		(insert org-notes-tab)
		(insert beg-str)
		(insert "v\n")
		(insert org-notes-tab)
		(insert beg-str)
		(org-notes--add-cur-pos-to-list)
		(org-notes--insert-redraw-button org-notes--current-expanded-child)
		(insert "\n")
		(insert "Children:")
		(mapcar
		 (lambda (node)
		   (progn
		     (insert "  ")
		     (org-notes--add-cur-pos-to-list)
		     (org-notes--insert-redraw-button node)))
		 (org-notes/node-children org-notes--current-expanded-child))))))
    (progn
      (insert ":\n")))
  (insert "WIP"))

(defun org-notes--redraw-buffer (cur-node cur-parent)
  (let ((inhibit-read-only t))
    (org-notes--refresh-buffer)
    (insert (org-notes--display-header))
    (if (= org-notes--current-mode org-notes/dired-mode)
	(org-notes--insert-dired cur-node cur-parent)
      (org-notes--insert-tree cur-node cur-parent))))


(defun org-notes-redisplay-from-node (node &optional MODE)
  (interactive)
  (if MODE
      (setq org-notes--current-mode MODE))
  (let ((cur-parent (org-notes/find-parent-node-of-node node))
	(cursor-idx org-notes--cur-pos-idx))
    (org-notes--redraw-buffer node cur-parent)
    (org-notes--set-pos-idx cursor-idx)))

(defun org-notes-visualize (&optional FILENAME MODE)
  (interactive)
  (unless FILENAME (setq FILENAME (buffer-file-name)))
  (setq org-notes/file-window (selected-window))

  (if (not (and
	    org-notes--current-notes-file
	    (file-equal-p FILENAME org-notes--current-notes-file)))
      (progn
	(setq org-notes--expanded-headings ())
	(setq org-notes--current-notes-file FILENAME)))
  (if MODE
      (setq org-notes--current-mode MODE))
  (setq org-notes--current-tree (org-notes-get-tree (org-notes--get-root-file FILENAME)))
  (let ((cur-parent (org-notes/find-node-file (org-notes--get-parent-file FILENAME)))
	(cur-node (org-notes/find-node-file FILENAME))
	(cur-entry (org-notes/find-node-name (org-entry-get nil "ITEM"))))
    (setq org-notes--current-notes-node cur-node)
    (select-window
     (display-buffer-in-side-window
      (get-buffer-create org-notes-buffer-name) '((side . right))))
    (set-buffer org-notes-buffer-name)
    (org-notes-mode)
    (org-notes--redraw-buffer cur-node cur-parent)
    (org-notes--set-pos-idx 0)))

(defun org-notes-display-dired (&optional FILENAME)
  (interactive)
  (org-notes-visualize FILENAME org-notes/dired-mode))

(defun org-notes-display-tree (&optional FILENAME)
  (interactive)
  (org-notes-visualize FILENAME org-notes/tree-mode))


(defvar org-notes-mode-map nil "Keymap for `org-notes-mode'")

(progn
  (setq org-notes-mode-map (make-sparse-keymap))
  (define-key org-notes-mode-map (kbd "q") 'kill-current-buffer)
  (define-key org-notes-mode-map (kbd "h") (lambda ()
					     (interactive)
					     (let ((cur-node (org-notes/find-node-file org-notes--current-notes-file)))
					       (message(org-notes--get-index-folder-path cur-node)))))
  (define-key org-notes-mode-map (kbd "n") 'org-notes--next-pos)
  (define-key org-notes-mode-map (kbd "p") 'org-notes--prev-pos)
  (define-key org-notes-mode-map (kbd "o") (lambda ()
					     (interactive)
					     (let ((cur-btn (button-at (point))))
					       (org-notes--follow-link-in-window
						(org-notes/node-file (button-get cur-btn 'node))))))
  (define-key org-notes-mode-map (kbd "a") (lambda ()
					     (interactive)
					     (let ((node-type (completing-read "Type of node: " (org-notes--make-add-node-selection-list) nil t))
						   (node-name (read-string "Name of node: ")))
					       (funcall (gethash node-type org-notes/add-node-hash) org-notes--current-notes-node node-name)
					       (org-notes-redisplay-from-node org-notes--current-notes-node))))
  (define-key org-notes-mode-map (kbd "m") (lambda ()
					     (interactive)
					     (setq POS (point))
					     (if (eq org-notes--current-mode org-notes/dired-mode)
						 (org-notes--toggle-heading-at-point-for-expand POS)
					       (org-notes--toggle-expanded-child POS))
					     (org-notes-redisplay-from-node org-notes--current-notes-node)
					     (goto-char POS)))
  (define-key org-notes-mode-map (kbd "t") (lambda ()
					     (interactive)
					     (org-notes-redisplay-from-node
					      org-notes--current-note-node
					      (org-notes/toggle-mode org-notes--current-mode))))
  (define-key org-notes-mode-map (kbd "r") (lambda ()
					     (interactive)
					     (org-notes-redisplay-from-node org-notes--current-notes-node)))
  (define-key org-notes-mode-map (kbd "e") 'org-notes--open-heading-to-edit))


(define-derived-mode org-notes-mode special-mode "notes"
  "org-notes-mode is a major mode for the org-notes buffer.")



(root-leader
  "b" '(:ignore t :which-key "org-notes"))

(root-leader
  "bd" 'org-notes-display-dired
  "bt" 'org-notes-display-tree)

(provide 'init-org-notes)
