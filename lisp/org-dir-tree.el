


(defun mattroot/org-kwd (KEYWORD)
  "get the value of a KEYWORD in the form of #+KEYWORD: value"
  (cdr (assoc KEYWORD (mattroot/org-kwds))))

(defun mattroot/org-kwd-file (FILE KEYWORD)
  ;; (setq-local cur-bfr (current-bmuffer))
  (setq-local bfr (get-file-buffer FILE))
  (with-current-buffer (if bfr
			   (set-buffer bfr)
			 (set-buffer (find-file-existing FILE)))
    (mattroot/org-kwd KEYWORD)))


(defconst org-dir-tree/parent-file-property "PARENT-FILE"
  "Property key for getting root dir from info org file.")
(defconst org-dir-tree/sub-index-file-property "NOTE-INDEX"
  "Property key for getting sub-index of heading.")

(cl-defstruct org-dir-tree/node name index-file-name props children)

(defun org-dir-tree/make-node (name index-file-name props children)
  "Helper for making an org-notes/node."
  (make-org-dir-tree/node :name name :index-file-name index-file-name :props props :children children))

(defun org-dir-tree--get-headings-of-file (FILE &optional ABSPATH)
  (if (file-exists-p FILE)
      (org-map-entries
       (lambda ()
	 (interactive)
	 (let ((path ABSPATH)
	       (position (point)))
	   (list
	    (org-entry-get nil "ITEM") ;; NAME 0
	    (if (org-entry-get nil org-dir-tree/sub-index-file-property)
		(concat path (org-entry-get nil org-dir-tree/sub-index-file-property))
	      nil) ;; SUB-INDEX File 1
	    (org-outline-level) ;; Outline Level 2
	    FILE ;; Current File 3
	    (append `(("POS" . ,position)) (org-entry-properties))))) ;; Properties 4
       nil
       (list FILE))))

(defun org-dir-tree--structure-tree-help (struct level)
  (let ((cur-node-list '()))
    (while (and struct (<= level (caddar struct)))
      (let* ((cur-l (car struct))
	     (new-struct (cdr struct))
	     (next-level (if new-struct (nth 2 (car new-struct)) nil)))
	(push
	 (org-dir-tree/make-node
	  (nth 0 cur-l) ;; NAME
	  (if (nth 1 cur-l)
	      (nth 1 cur-l)
	    (nth 3 cur-l)) ;; INDEX-FILE-NAME
	  (nth 4 cur-l)
	  (if (and
	       next-level
	       (< level next-level))
	      (let ((tmp-struct (org-dir-tree--structure-tree-help new-struct next-level)))
		(setq new-struct (car tmp-struct))
		(cadr tmp-struct))
	    (if (nth 1 cur-l)
		(org-dir-tree--structure-get-children (nth 1 cur-l))))) ;; CHILDREN
	 cur-node-list)
	(setq struct new-struct)))
    (list struct (reverse cur-node-list))))

(defun org-dir-tree--structure-get-children (FILENAME)
  (cadr (org-dir-tree--structure-tree-help
	 (org-dir-tree--get-headings-of-file
	  FILENAME
	  (file-name-directory FILENAME))
	 1)))

(defun org-dir-tree--structure-tree (FILENAME)
  (list (org-dir-tree/make-node
	 (mattroot/org-kwd-file FILENAME "TITLE")
	 FILENAME
	 nil
	 (cadr (org-dir-tree--structure-tree-help
		(org-dir-tree--get-headings-of-file
		 FILENAME
		 (file-name-directory FILENAME))
		1)))))

(defun org-dir-tree-get-tree (&optional FILENAME)
  (unless FILENAME (setq FILENAME (buffer-file-name)))
  (if FILENAME
      (org-dir-tree--structure-tree FILENAME)))


;;;;;;;;;;;;
;; SEARCHING
;;;;;;;;;;;;

(defun org-dir-tree/find-node--helper (func tree)
  (if tree
      (let ((current-node (car tree)))
	(if (funcall func current-node)
	    current-node
	  (let ((node (org-dir-tree/find-node--helper func (org-dir-tree/node-children current-node))))
	    (if node
		node
	      (org-dir-tree/find-node--helper func (cdr tree))))))))

(defun org-dir-tree/find-node (comp &optional tree)
  (org-dir-tree/find-node--helper
   comp
   tree))

;; (defun org-dir-tree/find-node-file (comp &optional tree)
;;   (unless tree (setq-local tree org-dir-tree--current-tree))
;;   (org-dir-tree/find-node--helper
;;    comp
;;    tree))

(defun org-dir-tree/find-node-file (file &optional tree)
  (if file
      (let ((file file))
	(org-dir-tree/find-node
	 (lambda (node)
	   (file-equal-p file (org-dir-tree/node-index-file-name node)))
	 tree))))

(defun org-dir-tree/find-in-children (func node)
  (let ((res nil))
    (dolist
	(n (org-dir-tree/node-children node) res)
      (setq res (or res (funcall func n))))))

(defun org-dir-tree/find-in-children-name (name node)
  (let ((name name))
    (org-dir-tree/find-in-children
     (lambda (n)
       (string-equal name (org-dir-tree/node-name n)))
     node)))

(defun org-dir-tree/is-child (node child-node)
  (let ((res nil))
    (dolist
	(n (org-dir-tree/node-children node) res)
      (setq res (or res (equal n child-node))))))

(defun org-dir-tree/find-parent-node--helper (func tree)
  (if tree
      (let ((current-node (car tree)))
	(if (org-dir-tree/find-in-children func current-node)
	    current-node
	  (let ((node (org-dir-tree/find-parent-node--helper func (org-dir-tree/node-children current-node))))
	    (if node
		node
	      (org-dir-tree/find-parent-node--helper func (cdr tree))))))))

(defun org-dir-tree/find-parent-node (func root)
  (org-dir-tree/find-parent-node--helper func root))

(defun org-dir-tree/find-parent-node-of-node (node root)
  (let ((curnode node))
    (org-dir-tree/find-parent-node
     (lambda (node)
       (equal curnode node))
     root)))

(defun org-dir-tree/is-leaf (node)
  (if (org-notes/node-children node) nil t))

(defun org-dir-tree--get-root-file (&optional FILENAME)
  (unless FILENAME (setq FILENAME (buffer-file-name)))
  (let ((prnt-file (org-dir-tree--get-parent-file FILENAME)))
    (if prnt-file
	(org-notes--get-root-file prnt-file)
      FILENAME)))

(defun org-dir-tree--get-parent-file (&optional FILENAME)
  (unless FILENAME (setq FILENAME (buffer-file-name)))
  (mattroot/org-kwd-file FILENAME org-dir-tree/parent-file-property))


(provide 'org-dir-tree)
