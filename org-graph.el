;;; org-graph.el --- org node graph -*- lexical-binding: t -*-

;; Copyright (C) 2023 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 26th January 2023
;; Package-requires: ((dash "2.17.0") (org "9.6") (ov "0") (s "0"))
;; Keywords: outlines

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'mule-util)

(require 'dash)
(require 'ov)
(require 's)

(require 'org)
(require 'org-refile)
(require 'org-agenda)
(require 'org-element)
(require 'org-fold)

(defgroup org-graph nil
  "Options concerning `org-graph'."
  :tag "Org Graph"
  :group 'org)

(defcustom org-graph-targets org-refile-targets
  "Specify all the valid org-graph targets.

The format of this variable is same as `org-refile-targets'."
  :group 'org-graph
  :type '(repeat
          (cons
           (choice :value org-agenda-files
             (const :tag "All agenda files" org-agenda-files)
             (const :tag "Current buffer" nil)
             (function) (variable) (file))
           (choice :tag "Identify target headline by"
             (cons :tag "Specific tag" (const :value :tag) (string))
             (cons :tag "TODO keyword" (const :value :todo) (string))
             (cons :tag "Regular expression" (const :value :regexp) (regexp))
             (cons :tag "Level number" (const :value :level) (integer))
             (cons :tag "Max Level number" (const :value :maxlevel) (integer))))))

(defcustom org-graph-dir
  (expand-file-name
   (convert-standard-filename "var/org-graph/")
   user-emacs-directory)
  "Directory where GPT embeddings are cached."
  :group 'org-graph
  :type 'directory)

(defcustom org-graph-embeddings-cache-dir
  (expand-file-name
   (convert-standard-filename "embeddings/")
   org-graph-dir)
  "Directory where GPT embeddings are cached."
  :group 'org-graph
  :type 'directory)

(defvar org-graph-cache nil
  "Cache for graph targets.")

(defvar org-graph-breadcrumbs nil
  "List of previously visited entries.

The `car' of this list is the current entry.  When we move back,
we pop from the start of the list.  When we visit more buffers,
we push to the front.")

(defun org-graph--next-heading ()
  "Go to next heading or end of file if at the last heading.

Return point."
  (or (outline-next-heading) (goto-char (point-max)))
  (point))

(defun org-graph-clear-cache ()
  "Clear org-graph cache."
  (let ((org-refile-cache org-graph-cache))
    (org-refile-cache-clear)))

(defun org-graph-target (prompt &optional arg)
  (when (equal arg '(64))
    (org-graph-clear-cache)
    (user-error "Cleared org-graph target cache"))
  (let ((org-refile-target-verify-function nil)
        (org-refile-targets org-graph-targets)
        (org-refile-cache org-graph-cache))
    (prog1 (org-refile-get-location prompt)
      (setq org-graph-cache org-refile-cache))))

(defun org-graph-get-files ()
  "Produce a list of org graph files.

The files considered by org-graph are defined using
`org-graph-targets'."
  (let ((files nil)
        (target-files nil))
    (dolist (entry org-graph-targets)
      (setq files (car entry))
      (cond
       ((null files) (push (list (buffer-file-name)) target-files))
       ((eq files 'org-agenda-files)
        (push (org-agenda-files 'unrestricted) target-files))
       ((and (symbolp files) (fboundp files))
        (push (funcall files) target-files))
       ((and (symbolp files) (boundp files))
        (push (symbol-value files) target-files)))
      (when (stringp files) (push (list files) target-files)))
    (delete-dups (nreverse (-flatten target-files)))))

(defvar org-graph-backlinks-cache (make-hash-table :test #'equal))
(defvar org-graph-backlinks-grouped-cache nil)

(defun org-graph--collect-backlinks ()
  (let ((backlinks)
        (cache-updated nil))
    (dolist (file (org-graph-get-files))
      (with-current-buffer (find-file-noselect file)
        (let ((sha1 (sha1 (buffer-substring-no-properties (point-min) (point-max))))
              (cache-entry (map-elt org-graph-backlinks-cache file))
              (file-backlinks))
          (if (or (not cache-entry)
                  (not (equal (plist-get cache-entry :sha1) sha1)))
              (when (eq major-mode 'org-mode)
                (save-excursion
                  (goto-char (point-min))
                  (while (re-search-forward org-bracket-link-regexp nil t)
                    (let ((link (match-string-no-properties 1)))
                      (when (and link (string-prefix-p "id:" link))
                        (push (list (substring link 3)
                                    (org-id-get-create))
                              file-backlinks))))
                  (map-put! org-graph-backlinks-cache file
                            (list :sha1 sha1
                                  :backlinks file-backlinks))
                  (setq cache-updated t)))
            (setq file-backlinks (plist-get cache-entry :backlinks)))
          (dolist (backlink file-backlinks)
            (push backlink backlinks)))))
    (if (or cache-updated
            (not org-graph-backlinks-grouped-cache))
        (let ((re (--map (cons (car it) (mapcar #'cadr (cdr it)))
                         (-group-by #'car backlinks))))
          (setq org-graph-backlinks-grouped-cache re))
      org-graph-backlinks-grouped-cache)))

(defun  org-graph--resolve-pom (&optional pom)
  "Resolve POM.

If POM is a list, it is assumed to be a plist and we extract the
:pom key.

If POM is a number-or-marker it is returned.

If POM is nil, `point-marker' is returned."
  (or (and (listp pom)
           (plist-get pom :pom))
      pom
      (point-marker)))

(defun org-graph--make-node (pom)
  "Make node at POM"
  (when (stringp pom)
    (setq pom (org-id-find pom 'marker)))
  (org-with-point-at pom
    (list :pom pom
          :id (org-id-get-create)
          :name (org-get-heading 'no-tags 'no-todo)
          :tags (mapcar #'substring-no-properties
                        (org-get-tags nil 'local)))))

;;; Parents

(defun org-graph--get-parent-from-tree (&optional pom get-all)
  "Get the first outline parent of the headline at POM.

If GET-ALL is non-nil, get all the parents from tree.

If any of the parents has the property GRAPH_PARENT_SKIP this
parent is not included in the parents but its parents *are*
traversed.

If any of the parents (including this entry) has the property
GRAPH_PARENT_ROOT all the parents above this headline will be
ignored.  This root parent is included, so if you want to skip it
as well give it GRAPH_PARENT_SKIP property as well.

Return marker pointing to the first eligible parent entry."
  (org-with-point-at pom
    (let* ((org-agenda-skip-function-global nil)
           (parents nil)
           (parent
            (catch 'done
              (when (org-entry-properties nil "GRAPH_PARENT_ROOT")
                (throw 'done nil))
              (while (org-up-heading-safe)
                (unless (org-entry-properties nil "GRAPH_PARENT_SKIP")
                  (if get-all
                      (push (point-marker) parents)
                    (throw 'done (point-marker))))
                (when (org-entry-properties nil "GRAPH_PARENT_ROOT")
                  (throw 'done nil))))))
      (or parent parents))))

(defun org-graph--get-parents-from-property (&optional pom)
  "Get all parents specified in the GRAPH_PARENTS property at POM.

Return list of markers pointing to the parent entries."
  (org-with-point-at pom
    (let ((parents (org-entry-get-multivalued-property nil "GRAPH_PARENTS")))
      (-map (lambda (entry) (org-id-find entry 'marker)) parents))))

(defun org-graph-get-parents (&optional pom tree-all)
  "Return all parents at POM.

If POM is a list, first extract the :pom property and use that.

If TREE-ALL is non-nil, retrieve all parents from the tree."
  (setq pom (org-graph--resolve-pom pom))
  (let* ((parents-from-property (org-graph--get-parents-from-property pom))
         (parent-from-tree (org-graph--get-parent-from-tree pom tree-all))
         (prop-parents (-map #'org-graph--make-node parents-from-property))
         (tree-parents (-map #'org-graph--make-node (-list parent-from-tree))))
    (append
     (mapcar (lambda (p) (plist-put p :from-prop t)) prop-parents)
     (mapcar (lambda (p) (plist-put p :from-tree t)) tree-parents))))

(defun org-graph-add-parent (&optional pom arg)
  (interactive (list (point) current-prefix-arg))
  (org-with-point-at pom
    (-when-let (parent (-last-item (org-graph-target "Parent: " arg)))
      (let ((my-id (org-id-get-create))
            (parent-id (org-with-point-at parent (org-id-get-create))))
        (org-entry-add-to-multivalued-property parent "GRAPH_CHILDREN" my-id)
        (org-entry-add-to-multivalued-property pom "GRAPH_PARENTS" parent-id)))))

;;; Children
(defun org-graph--get-children-from-tree (&optional pom)
  "Get all children of the headline at POM.

If any of the children has the property GRAPH_CHILD_SKIP this
child is not included in the children.

If any of the headlines has the property GRAPH_CHILD_LEAF no
children of that headline will be included.  In other words,
calling this function at such a headline will return no children.

Return list of markers pointing to the child entries."
  (org-with-point-at pom
    (org-back-to-heading t)
    (setq pom (point-marker))
    (unless (org-entry-get (point) "GRAPH_CHILD_LEAF")
      (let* ((org-agenda-skip-function-global nil)
             (re))
        (save-restriction
          (widen)
          (org-back-to-heading t)
          ;; we have to setup our own restrictions, because
          ;; `org-map-entries' with 'tree restriction restricts one
          ;; less char than necessary and then the last headline is
          ;; picked up even if it is nested more levels than the
          ;; current node's children.
          (narrow-to-region
           (point)
           (save-excursion (org-end-of-subtree t t)))
          (org-map-entries
           'point-marker t nil
           (lambda ()
             (unless (org-entry-properties nil "GRAPH_CHILD_SKIP")
               (push (point-marker) re))
             (unless (= (point) pom)
               (save-excursion (org-end-of-subtree t t))))))
        (setq re (nreverse re))
        (when (= (car re) pom) (pop re))
        re))))

(defun org-graph--get-children-from-property (&optional pom)
  "Get all children specified in the GRAPH_CHILDREN property at POM.

Return list of markers pointing to the child entries."
  (org-with-point-at pom
    (let ((children (org-entry-get-multivalued-property nil "GRAPH_CHILDREN")))
      (-map (lambda (entry) (org-id-find entry 'marker)) children))))

(defun org-graph-get-children (&optional pom b)
  "Return all children at POM.

If POM is a list, first extract the :pom property and use that."
  (setq pom (org-graph--resolve-pom pom))
  (let ((children-from-property (org-graph--get-children-from-property pom))
        (children-from-tree (org-graph--get-children-from-tree pom)))
    (-map #'org-graph--make-node
          (-concat children-from-tree children-from-property))))

(defun org-graph-add-child (&optional pom arg)
  (interactive (list (point) current-prefix-arg))
  (org-with-point-at pom
    (-when-let (child (-last-item (org-graph-target "Child: " arg)))
      (let ((my-id (org-id-get-create))
            (child-id (org-with-point-at child (org-id-get-create))))
        (org-entry-add-to-multivalued-property child "GRAPH_PARENTS" my-id)
        (org-entry-add-to-multivalued-property pom "GRAPH_CHILDREN" child-id)))))

(defun org-graph-insert-link (&optional arg)
  "Insert a link to any valid target node."
  (interactive "P")
  (-when-let (target (-last-item (org-graph-target "Target: " arg)))
    (let* ((target-node (org-graph--make-node target)))
      (org-insert-link
       nil
       (format "id:%s" (plist-get target-node :id))
       (plist-get target-node :name)))))

;;; Siblings

(defun org-graph-get-siblings (&optional pom)
  "Get siblings of the entry at POM."
  (setq pom (org-graph--resolve-pom pom))
  (org-with-point-at pom
    (org-back-to-heading t)
    (let* ((pom (point-marker))
           (parents (org-graph-get-parents pom))
           (siblings (-mapcat (-lambda ((&plist :pom pom))
                                (org-graph-get-children pom))
                              parents)))
      (let ((-compare-fn (-lambda ((&plist :pom pom1)
                                   (&plist :pom pom2))
                           (equal pom1 pom2))))
        (-remove (-lambda ((&plist :pom p))
                   (equal p pom))
                 (-uniq siblings))))))

(defun org-graph-leaf-p (&optional pom)
  (setq pom (org-graph--resolve-pom pom))
  (org-entry-get pom "GRAPH_CHILD_LEAF"))

(defun org-graph-set-leaf (&optional pom)
  (interactive)
  (setq pom (or pom (point-marker)))
  (org-entry-put pom "GRAPH_CHILD_LEAF" "t"))

(defun org-graph-set-root (&optional pom)
  (interactive)
  (setq pom (or pom (point-marker)))
  (org-entry-put pom "GRAPH_CHILD_ROOT" "t"))

(defun org-graph-set-parent-skip (&optional pom)
  (interactive)
  (setq pom (or pom (point-marker)))
  (org-entry-put pom "GRAPH_PARENT_SKIP" "t"))

(defun org-graph-set-child-skip (&optional pom)
  (interactive)
  (setq pom (or pom (point-marker)))
  (org-entry-put pom "GRAPH_CHILD_SKIP" "t"))

(defun org-graph--read-relative (prompt collection)
  "Read a relative with PROMPT from COLLECTION."
  (let* ((collection-alist
          (-map
           (lambda (p)
             (org-with-point-at (plist-get p :pom)
               (cons
                (format "%s/%s"
                        (org-get-category)
                        (s-join "/" (org-get-outline-path 'with-self)))
                p)))
           collection))
         (choice (if (= (length collection) 1)
                     (caar collection-alist)
                   (completing-read prompt collection-alist nil t
                                    nil nil (caar collection-alist)))))
    (plist-get (cdr (assoc choice collection-alist)) :id)))

(defun org-graph-goto-parent (&optional pom)
  (interactive)
  (setq pom (or pom (point-marker)))
  (if-let* ((parents (org-graph-get-parents pom)))
      (let ((id (org-graph--read-relative "Parent: " parents)))
        (if id (org-id-goto id)
          (user-error "Parent node not found")))
    (user-error "Node has no parents")))

(defun org-graph-goto-child (&optional pom)
  (interactive)
  (setq pom (or pom (point-marker)))
  (if-let* ((children (org-graph-get-children pom)))
      (let ((id (org-graph--read-relative "Child: " children)))
        (if id (org-id-goto id)
          (user-error "Child node not found")))
    (user-error "Node has no children")))

(defun org-graph--extract-special-element (pom type filter &optional as-element)
  "Return first element of TYPE in subtree at POM matching FILTER.

Element is returned as a string."
  (declare (indent 1))
  (org-with-point-at pom
    (save-excursion
      (save-restriction
        (widen)
        (org-narrow-to-subtree)
        (let ((max (save-excursion (org-graph--next-heading))))
          (org-element-map (org-element-parse-buffer) type
            (lambda (elem)
              (let ((prop (cadr elem)))
                (when (and (< (plist-get prop :begin) max)
                           (funcall filter prop))
                  (if as-element elem
                    (buffer-substring-no-properties
                     (plist-get prop :contents-begin)
                     (plist-get prop :contents-end))))))
            nil
            'first-match))))))

;;; Graphics
;; From https://github.com/alphapapa/unpackaged.el
(defun org-graph--forward-to-entry-content (&optional unsafe)
  "Skip headline, planning line, and all drawers in current entry.
If UNSAFE is non-nil, assume point is on headline."
  (unless unsafe
    ;; To improve performance in loops (e.g. with `org-map-entries')
    (ignore-errors (org-back-to-heading)))
  (let ((max-pos (save-excursion (org-graph--next-heading)))
        (pos (point)))
    (while (and pos (< pos max-pos))
      (when-let* ((element (org-element-at-point)))
        (setq pos
              (pcase element
                (`(headline . ,_) (org-element-property :contents-begin element))
                (`(,(or 'planning 'property-drawer 'drawer) . ,_) (org-element-property :end element))))
        (when pos (goto-char pos))))))

(defun org-graph--get-headline-content ()
  "Get content of current headline"
  (let ((max-pos (save-excursion (org-graph--next-heading))))
    (save-excursion
      (org-graph--forward-to-entry-content)
      (buffer-substring-no-properties (point) max-pos))))

(cl-defun org-graph--render-link (item &optional relatives
                                       &key (width 40))
  (when (stringp item)
    (setq item (org-id-find item 'marker)))
  (when (markerp item)
    (setq item (org-graph--make-node item)))

  (let* ((id (plist-get item :id))
         (name (plist-get item :name))
         (relatives (-list relatives))
         (parents-list
          (org-graph-get-parents item 'tree-all))
         (tree (--filter
                (plist-get it :from-tree)
                parents-list))
         (children-list
          (when (memq 'children relatives)
            (org-graph-get-children item))))
    (propertize
     (format "[[org-node-graph:%s][%s]]"
             (substring-no-properties id)
             (->> (truncate-string-to-width name width nil nil t)
                  (substring-no-properties)
                  ;; sanitize the brackets [] into {}
                  (replace-regexp-in-string "\\[" "{")
                  (replace-regexp-in-string "\\]" "}")))
     (intern id) t
     'org-node-graph-id id
     'org-node-graph-name
     (s-join " / "
             (-snoc (--map-indexed
                     (propertize
                      (substring-no-properties (plist-get it :name))
                      'face
                      `(quote ,(intern
                                (format "org-level-%d" (1+ it-index))))
                      'fontified t)
                     tree)
                    (propertize
                     (substring-no-properties name)
                     'face
                     `(quote ,(intern (format "org-level-%d" (1+ (length tree)))))
                     'fontified t)))
     'org-node-graph-parents
     (when (memq 'parents relatives)
       (-uniq (-map (-lambda ((&plist :id)) id) parents-list)))
     'org-node-graph-children
     (-uniq (-map (-lambda ((&plist :id)) id) children-list)))))

(defcustom org-graph-rendered-properties
  '(
    ("KEYWORDS" . :multi)
    ("SOURCE" . :single)
    ("AUTHOR" . :single)
    ("PUBLISHED" . :single)
    )
  "Properties rendered on the node page."
  :type 'alist
  :group 'org-graph)

(defun org-graph--render-properties (pom)
  "Render PROPERTIES.

Properties is an ALIST of (KEY . PROPERTY).

PROPERTY can be a list, the items will be joined by a comma."
  (let* ((definitions
           (--filter
            (cdr it)
            (-map
             (-lambda ((name . type))
               (cond
                ((eq type :single)
                 (cons name (org-entry-get pom name)))
                ((eq type :multi)
                 (cons name (org-entry-get-multivalued-property pom name)))
                (t (error "Only :single or :multi type is supported"))))
             org-graph-rendered-properties)))
         (max-length (when definitions
                       (-max (-map #'length (-map #'car definitions)))))
         (rendered-props nil))

    (--each definitions
      (push
       (format (format "- %%-%ds :: %%s\n" max-length)
               (car it)
               (if (listp (cdr it))
                   (mapconcat #'identity (cdr it) ", ")
                 (cdr it)))
       rendered-props))

    (nreverse rendered-props)))

(defun org-graph--render-logbook (pom elem)
  "Render logbook element ELEM."
  (let* ((prop (cadr (nth 2 elem)))
         (items (plist-get prop :structure)))
    (org-with-point-at pom
      (save-match-data
        (save-excursion
          (s-join
           "\n"
           (-keep (lambda (item)
                    (goto-char (car item))
                    (when (looking-at "- Note taken on \\[\\(.*\\)\\] \\\\\\\\$")
                      (forward-line)
                      (concat
                       "- [" (match-string 1) "] "
                       (buffer-substring-no-properties
                        (point) (nth 6 item)))))
                  items)))))))

(defun org-graph--id-goto (id)
  "Pop to the buffer containing the entry with id ID.
Move the cursor to that entry in that buffer."
  (let ((m (org-id-find id 'marker)))
    (unless m
      (user-error "Cannot find entry with ID \"%s\"" id))
    (pop-to-buffer (marker-buffer m))
    (goto-char m)
    (move-marker m nil)
    (org-fold-show-context)))

(defvar-local org-graph-current-entry nil)
(defvar-local org-graph-current-query-data nil)

(defun org-graph-goto-current ()
  "Visit current entry in its org buffer."
  (interactive)
  (org-graph--id-goto (plist-get org-graph-current-entry :id)))

(defun org-graph-back ()
  "Visit previously visited node.

This uses `org-graph-breadcrumbs' for navigation."
  (let ((current (pop org-graph-breadcrumbs))
        (previous (pop org-graph-breadcrumbs)))
    (condition-case err
        (org-graph-render-node (plist-get previous :pom))
      (error
       (push previous org-graph-breadcrumbs)
       (push current org-graph-breadcrumbs)))))

(defun org-graph-revert-buffer ()
  "Revert current node graph buffer."
  (interactive)
  (cond
   (org-graph-current-entry
    (org-graph-render-node (plist-get org-graph-current-entry :pom)))
   (org-graph-current-query-data
    (org-graph-render-query
     (plist-get org-graph-current-query-data :query)
     (plist-get org-graph-current-query-data :results)))))

(defun org-graph-render-node (&optional pom)
  (interactive (list (nth 3 (org-graph-target "Show node"))))
  (setq pom (org-graph--resolve-pom pom))
  (let* ((current-node (org-graph--make-node pom))
         (buffer (get-buffer-create "*org-node-graph*"))
         (parents (progn
                    (message "Getting parents ...")
                    (org-graph-get-parents pom)))
         (children (progn
                     (message "Fetching children ...")
                     (org-graph-get-children pom)))
         (siblings (progn
                     (message "Fetching siblings ...")
                     (org-graph-get-siblings pom)))
         (backlinks (progn
                      (message "Collecting backlinks ...")
                      (mapcar
                       #'org-graph--make-node
                       (cdr
                        (assoc
                         (plist-get current-node :id)
                         (org-graph--collect-backlinks))))))
         (logbook
          (org-graph--extract-special-element pom
            'drawer
            (lambda (prop)
              (equal (plist-get prop :drawer-name) "LOGBOOK"))
            :as-element))
         (abstract
          (org-graph--extract-special-element pom
            'special-block
            (lambda (prop)
              (equal (plist-get prop :type) "ABSTRACT"))))
         (resources
          (org-graph--extract-special-element pom
            'drawer
            (lambda (prop)
              (equal (plist-get prop :drawer-name) "RESOURCES")))))

    (unless (equal (plist-get current-node :id)
                   (plist-get (car org-graph-breadcrumbs) :id))
      (push current-node org-graph-breadcrumbs))

    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (org-mode)
      (org-graph-mode 1)
      (variable-pitch-mode -1)
      (setq-local buffer-face-mode-face '(:family "DejaVu Sans Mono" :height 140))
      (buffer-face-mode 1)

      (when (cdr org-graph-breadcrumbs)
        (let ((i 0))
          (insert "Breadcrumbs: ")
          (--each (cdr org-graph-breadcrumbs)
            (cl-incf i)
            (message "Rendering breadcrumbs %d/%d" i (length (cdr org-graph-breadcrumbs)))
            (insert (org-graph--render-link it))
            (when (< i (length (cdr org-graph-breadcrumbs)))
              (insert " > ")))
          (insert "\n\n")))

      (when (> (length parents) 0)
        (let ((i 0))
          (--each parents
            (cl-incf i)
            (message "Rendering parents %d/%d" i (length parents))
            (insert (org-graph--render-link it 'children) "    ")))
        (insert "\n│\n"))

      (insert (if siblings "├" "└"))

      (insert " ["
              (org-graph--render-link current-node nil :width 38)
              "]\n")
      (let ((i 0))
        (--each siblings
          (cl-incf i)
          (message "Rendering siblings %d/%d" i (length siblings))
          (insert (if (< i (length siblings)) "├ " "└ ")
                  (org-graph--render-link it '(children parents))
                  "\n")))
      (insert "\n")
      (when (> (length children) 0)
        (goto-char (point-min))
        (forward-line 1)
        (goto-char (text-property-any
                    (point) (point-max)
                    (intern (plist-get current-node :id)) t))
        (end-of-line)
        (font-lock-ensure (point-min) (point-max))
        (insert " " (make-string (max (- 42 (current-column)) 0) ?─))
        (insert-rectangle
         (let ((i 0))
           (-concat
            (--map (progn
                     (cl-incf i)
                     (message "Rendering children %d/%d" i (length children))
                     (concat (cond ((= i 1) (if (= i (length children)) "─ " "┬ "))
                                   ((= i (length children)) "└ ")
                                   (t "├ "))
                             (org-graph--render-link it 'parents)))
                   children)
            (list "" "")))))

      (goto-char (point-max))
      (beginning-of-line)

      (when (> (length backlinks) 0)
        (insert "- Backlinks :: ")
        (--each backlinks
          (insert (org-graph--render-link it '(children parents)) "    "))
        (insert "\n\n"))

      (when-let ((rendered-props (org-graph--render-properties pom)))
        (insert (s-join "" rendered-props) "\n"))

      (when resources
        (insert "* Resources\n"
                resources
                "\n\n"))

      (when abstract
        (insert "* Abstract\n"
                abstract
                "\n\n"))

      (when logbook
        (when-let ((logbook-rendered (org-graph--render-logbook pom logbook)))
          (insert "* Logbook\n" logbook-rendered "\n\n")
          (save-excursion
            (org-mark-subtree)
            (call-interactively 'org-fill-paragraph))))

      ;; insert the node directly
      (let ((tree nil))
        (org-with-point-at pom
          (let (kill-ring)
            ;; For leaf nodes, insert everything, for non-leaf nodes,
            ;; only insert the content up to the next heading.  If the
            ;; content is empty, do not insert anything.
            (shut-up (org-copy-subtree nil nil nil (not (org-graph-leaf-p))))
            (setq tree org-subtree-clip)
            (let ((content (org-graph--get-headline-content)))
              (when (and (string-match-p "\\` *\\'" content)
                         (not (org-graph-leaf-p)))
                (setq tree nil)))))
        (when tree
          (insert "* Node\n" tree))
        (org-cycle-hide-drawers 'all))

      (message "org-graph-render-node ... done")

      (put-text-property (point-min) (point-max) 'help-echo 'org-graph-help-echo)
      (goto-char (point-min))

      (read-only-mode 1)
      (setq-local org-graph-current-entry current-node))))

(defun org-graph-cursor-sensor (window _old dir)
  (if (eq dir 'left)
      (ov-clear 'org-node-graph)
    (org-graph-highlight (window-point window))))

(defun org-graph-help-echo (window _object position)
  (if (or (get-text-property position 'org-node-graph-parents)
          (get-text-property position 'org-node-graph-children))
      (with-selected-window window
        (org-graph-highlight position))
    (ov-clear 'org-node-graph))
  (-if-let (name (get-text-property position 'org-node-graph-name))
      name
    ""))

(defun org-graph-highlight (point)
  (ov-clear 'org-node-graph)
  (cl-flet ((render-relative
             (sym-id relation)
             (save-excursion
               (goto-char (point-min))
               (let ((beg nil) (end t))
                 (while (and (setq beg (text-property-any
                                        (point) (point-max)
                                        sym-id t))
                             end)
                   (setq end (next-single-property-change beg sym-id))
                   (goto-char (1+ end))
                   (ov beg end
                       'org-node-graph t
                       'face (cond
                              ((eq relation 'parent)
                               '(:background "#204a87"))
                              ((eq relation 'child)
                               '(:background "#5c3566")))))))))
    (save-excursion
      (--each (get-text-property point 'org-node-graph-parents)
        (render-relative (intern it) 'parent))
      (--each (get-text-property point 'org-node-graph-children)
        (render-relative (intern it) 'child)))))


(org-link-set-parameters
 "org-node-graph"
 :follow (lambda (id) (org-graph-render-node (org-id-find id 'marker)))
 :activate-func (lambda (beg end _path _ignored)
                  (add-text-properties
                   beg end
                   (list
                    'cursor-sensor-functions (list 'org-graph-cursor-sensor))))
 :help-echo 'org-graph-help-echo)

(defvar org-graph-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'org-graph-revert-buffer)
    (define-key map (kbd "o") 'org-graph-goto-current)
    (define-key map (kbd "l") 'org-graph-back)
    (define-key map (kbd "s") 'isearch-forward-regexp)
    (define-key map (kbd "r") 'isearch-backward-regexp)
    (define-key map (kbd "q") 'bury-buffer)
    map))

(define-minor-mode org-graph-mode
  "Minor mode for org-graph."
  :lighter " org-graph"
  :keymap org-graph-mode-map)

(provide 'org-graph)
;;; org-graph.el ends here
