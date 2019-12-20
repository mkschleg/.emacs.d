

(require 'org)

;; (defcustom )
(defgroup org-notes nil
  "Options in relation to org-notes in emacs"
  :tag "Org Notes"
  :group 'org)

(defcustom org-notes-tab "  "
  "Tab characters used for indenting the dir view of note."
  :group 'org-notes
  :type 'string)

(defcustom org-notes-follow-link-and-quit t
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

(setq org-notes--expanded-headings ())

(setq org-notes--previous-notes-file nil)

(defconst org-notes/root-dir-property "ROOT-DIR"
  "Property key for getting root dir from info org file.")
(defconst org-notes/root-file-property "ROOT-FILE"
  "Property key for getting root dir from info org file.")
(defconst org-notes/parent-file-property "PARENT-FILE"
  "Property key for getting root dir from info org file.")


(defun org-notes-get-link ()
  (org-entry-get nil "NOTE-INDEX"))

(defun org-notes--follow-link-in-window (link &optional window)
  (unless window (setq window org-notes/current-window))
  (setq org-notes/view-window (selected-window))
  (select-window window)
  (find-file-existing link)
  (if org-notes-follow-link-and-quit
      (kill-buffer org-notes-buffer-name)
    (select-window org-notes/view-window)))

(defun org-notes-follow-entry-link ()
  (interactive)
  (find-file-existing (org-notes-get-link)))

(defun org-notes--get-structure-of-current-buffer ()
  (org-map-entries
   (lambda ()
     (mapcar
      (lambda (itm) (org-entry-get nil itm))
      '("ITEM" "NOTE-INDEX")))
   "LEVEL=1"))

(defun org-notes--get-structure-of-file (FILE &optional ABSPATH)
  (if (file-exists-p FILE)
      (org-map-entries
       (lambda ()
	 (interactive)
	 (let ((path ABSPATH))
	   (if (org-entry-get nil "NOTE-INDEX")
	       (list
		(org-entry-get nil "ITEM")
		(concat path (org-entry-get nil "NOTE-INDEX")))
	     (list (org-entry-get nil "ITEM") nil))
	   ))
       "LEVEL=1"
       (list FILE))))

(defun org-notes--get-structure-of-files (FILES)
  (org-map-entries
   (lambda ()
     (mapcar
      (lambda (itm) (org-entry-get nil itm))
      '("ITEM" "NOTE-INDEX"))
     )
   "LEVEL=1"
   FILES))


(defun org-notes-get-structure (FILENAME)
  (if FILENAME
      (org-notes--get-structure-of-file FILENAME (file-name-directory FILENAME))))

(cl-defstruct org-notes/node name file children)

(defun org-notes/make-node (name file children)
  (make-org-notes/node :name name :file file :children children))

(defun org-notes-tree (FILENAME)
  (mapcar
   (lambda (struct)
     (interactive)
     (let ((file FILENAME))
       (org-notes/make-node
	(car struct)
	(if (cadr struct)
	    (cadr struct)
	  file)
	(org-notes-tree (cadr struct))
	)))
   (org-notes-get-structure FILENAME)))


(defun org-notes/find-node--helper (name tree)
  (if tree
      (let ((current-node (car tree)))
	(if (string-equal (org-notes/node-name current-node) name)
	    current-node
	  (let ((node (org-notes/find-node--helper name (org-notes/node-children current-node))))
	    (if node
		node
	      (org-notes/find-node--helper name (cdr tree))))))))

(defun org-notes/find-node (name &optional tree)
  (unless tree (setq-local tree org-notes/current-tree))
  (org-notes/find-node--helper name tree))

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
    (if (cdr (assoc org-notes/root-file-property props))
	(cdr (assoc org-notes/root-file-property props))
      FILENAME)))

(defun org-notes--get-parent-file (&optional FILENAME)
  (unless FILENAME (setq FILENAME (buffer-file-name)))
  (let ((props (org-notes--get-file-directory-info FILENAME)))
    (if (cdr (assoc org-notes/root-file-property props))
	(cdr (assoc org-notes/root-file-property props))
      nil)))



(defun org-notes--display-header ()
  "Notes tree hierarchy: \n\n"
  )

(defun org-notes--format-tree (tree &optional TAB)
  (unless TAB (setq-local TAB ""))
  (if tree
      (concat TAB "_" (org-notes/node-name (car tree)) "_" "\n"
	      (org-notes--format-tree (org-notes/node-children (car tree)) (concat TAB org-notes-tab))
	      (org-notes--format-tree (cdr tree) TAB))
    ""))

(defun org-notes--insert-button (node)
  (insert-button
   (org-notes/node-name node)
   'action
   (lambda (btn)
     (message (format "Button Pressed %s"
		      (concat (button-label btn) " "
			      (org-notes/node-file (org-notes/find-node (button-label btn))))))
     (org-notes--follow-link-in-window
      (org-notes/node-file (org-notes/find-node (button-label btn)))))))

(defun org-notes--insert-button-tree (tree &optional TAB LEVEL)
  (unless TAB (setq-local TAB ""))
  ;; (unless LEVEL (setq-local LEVEL 100))
  (if tree ;;(and tree (or (not LEVEL) (<= LEVEL 0)))
      (progn
	(if (or (not LEVEL)
		(> LEVEL 0))
	    (progn
	      (insert TAB)
	      (org-notes--insert-button (car tree))
	      (insert "\n")))
	(org-notes--insert-button-tree
	 (org-notes/node-children (car tree))
	 (concat TAB org-notes-tab)
	 (if (member (org-notes/node-name (car tree)) org-notes--expanded-headings)
	     1
	   (if LEVEL
	       (- LEVEL 1))))
	(org-notes--insert-button-tree (cdr tree) TAB LEVEL))))


(defun org-notes--redraw-buffer ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (org-notes--display-header))
    (org-notes--insert-button-tree org-notes/current-tree "   " org-notes-initial-display-level)))

(defun org-notes-display-tree (&optional FILENAME)
  (interactive)
  (unless FILENAME (setq FILENAME (buffer-file-name)))
  
  (if (not (string-equal FILENAME org-notes--previous-notes-file))
      (progn
	(setq org-notes--expanded-headings ())
	(setq org-notes--previous-notes-file FILENAME)))
  (setq org-notes/current-window (selected-window))
  (select-window
   (display-buffer-in-side-window
    (get-buffer-create org-notes-buffer-name) '((side . right))))
  (set-buffer org-notes-buffer-name)
  (org-notes-mode)
  (setq-local org-notes/current-tree (org-notes-tree FILENAME))
  ;; (read-only-mode 1)
  (org-notes--redraw-buffer))

(defun org-notes-display-full-tree (&optional FILENAME)
  (interactive)
  (unless FILENAME (setq FILENAME (org-notes--get-root-file (buffer-file-name))))
  (if (not (string-equal FILENAME org-notes--previous-notes-file))
      (progn
	(setq org-notes--expanded-headings ())
	(setq org-notes--previous-notes-file FILENAME)))
  (setq org-notes/current-window (selected-window))
  (select-window
   (display-buffer-in-side-window
    (get-buffer-create org-notes-buffer-name) '((side . right))))
  (set-buffer org-notes-buffer-name)
  (org-notes-mode)
  (setq-local org-notes/current-tree (org-notes-tree FILENAME))
  ;; (read-only-mode 1)
  (org-notes--redraw-buffer))


(defun org-notes--toggle-heading-at-point-for-expand (&optional POS)
  (unless POS (setq POS (point)))
  (let ((cur-lbl (button-label (button-at POS))))
    (if (not org-notes--expanded-headings)
	(setq org-notes--expanded-headings (list cur-lbl))
	(if (member cur-lbl org-notes--expanded-headings)
	    (setq org-notes--expanded-headings (remove cur-lbl org-notes--expanded-headings))
	  (add-to-list 'org-notes--expanded-headings cur-lbl)))))


(defvar org-notes-mode-map nil "Keymap for `org-notes-mode'")

(progn
  (setq org-notes-mode-map (make-sparse-keymap))
  (define-key org-notes-mode-map (kbd "q") 'kill-current-buffer)
  (define-key org-notes-mode-map (kbd "n") (lambda ()
					     (interactive)
					     (next-logical-line)
					     (back-to-indentation)))
  (define-key org-notes-mode-map (kbd "p") (lambda ()
					     (interactive)
					     (previous-line)
					     (back-to-indentation)))
  (define-key org-notes-mode-map (kbd "m") (lambda ()
					     (interactive)
					     (setq POS (point))
					     (org-notes--toggle-heading-at-point-for-expand)
					     (org-notes--redraw-buffer)
					     (goto-char POS))))

(define-derived-mode org-notes-mode special-mode "notes"
  "org-notes-mode is a major mode for the org-notes buffer."
  )



