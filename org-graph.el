(require 'dash)
(require 'org)
(require 'org-fold)

(defun org-graph--next-heading ()
  "Go to next heading or end of file if at the last heading.

Return point."
  (or (outline-next-heading) (goto-char (point-max)))
  (point))

(defun org-graph-target (prompt)
  (let ((org-refile-target-verify-function nil))
    (org-refile-get-location prompt)))

(defun org-graph--resolve-pom (&optional pom)
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
  (org-with-point-at pom
    (list :pom pom
          :id (org-id-get-create)
          :name (org-get-heading 'no-tags 'no-todo))))

;;; Parents

(defun org-graph--get-parent-from-tree (&optional pom)
  "Get all parents of the headline at POM.

If any of the parents has the property GRAPH_PARENT_SKIP this
parent is not included in the parents but its parents *are*
traversed.

If any of the parents (including this entry) has the property
GRAPH_PARENT_ROOT all the parents above this headline will be
ignored.  This root parent is included, so if you want to skip it
as well give it GRAPH_PARENT_SKIP property as well.

Return marker pointing to the first eligible parent entry."
  (org-with-point-at pom
    (let ((org-agenda-skip-function-global nil))
      (catch 'done
        (when (org-entry-properties nil "GRAPH_PARENT_ROOT")
          (throw 'done nil))
        (while (org-up-heading-safe)
          (unless (org-entry-properties nil "GRAPH_PARENT_SKIP")
            (throw 'done (point-marker)))
          (when (org-entry-properties nil "GRAPH_PARENT_ROOT")
            (throw 'done nil)))))))

(defun org-graph--get-parents-from-property (&optional pom)
  "Get all parents specified in the GRAPH_PARENTS property at POM.

Return list of markers pointing to the parent entries."
  (org-with-point-at pom
    (let ((parents (org-entry-get-multivalued-property nil "GRAPH_PARENTS")))
      (-map (lambda (entry) (org-id-find entry 'marker)) parents))))

(defun org-graph-get-parents (&optional pom)
  "Return all parents at POM.

If POM is a list, first extract the :pom property and use that."
  (setq pom (or (and (listp pom)
                     (plist-get pom :pom))
                pom
                (point-marker)))
  (let ((parents-from-property (org-graph--get-parents-from-property pom))
        (parent-from-tree (org-graph--get-parent-from-tree pom)))
    (-map #'org-graph--make-node (if parent-from-tree
                                     (cons parent-from-tree parents-from-property)
                                   parents-from-property))))

(defun org-graph-add-parent (&optional pom)
  (interactive)
  (org-with-point-at pom
    (-when-let (parent (-last-item (org-graph-target "Parent: ")))
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
        (org-map-entries
         'point-marker t 'tree
         (lambda ()
           (unless (org-entry-properties nil "GRAPH_CHILD_SKIP")
             (push (point-marker) re))
           (unless (= (point) pom)
             (save-excursion (org-end-of-subtree t)))))
        (setq re (nreverse re))
        (when (= (car re) pom) (pop re))
        re))))

(defun org-graph--get-children-from-property (&optional pom)
  "Get all children specified in the GRAPH_CHILDREN property at POM.

Return list of markers pointing to the child entries."
  (org-with-point-at pom
    (let ((children (org-entry-get-multivalued-property nil "GRAPH_CHILDREN")))
      (-map (lambda (entry) (org-id-find entry 'marker)) children))))

(defun org-graph-get-children (&optional pom)
  "Return all children at POM.

If POM is a list, first extract the :pom property and use that."
  (setq pom (or (and (listp pom)
                     (plist-get pom :pom))
                pom
                (point-marker)))
  (let ((children-from-property (org-graph--get-children-from-property pom))
        (children-from-tree (org-graph--get-children-from-tree pom)))
    (-map #'org-graph--make-node
          (-concat children-from-tree children-from-property))))

(defun org-graph-add-child (&optional pom)
  (interactive)
  (org-with-point-at pom
    (-when-let (child (-last-item (org-graph-target "Child: ")))
      (let ((my-id (org-id-get-create))
            (child-id (org-with-point-at child (org-id-get-create))))
        (org-entry-add-to-multivalued-property child "GRAPH_PARENTS" my-id)
        (org-entry-add-to-multivalued-property pom "GRAPH_CHILDREN" child-id)))))

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
  (let* ((id (org-graph--read-relative
              "Parent: "
              (org-graph-get-parents pom))))
    (if id (org-id-goto id)
      (user-error "Parent node not found"))))

(defun org-graph-goto-child (&optional pom)
  (interactive)
  (setq pom (or pom (point-marker)))
  (let* ((id (org-graph--read-relative
              "Child: "
              (org-graph-get-children pom))))
    (if id (org-id-goto id)
      (user-error "Child node not found"))))

(defun org-graph--extract-special-element (pom type filter)
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
                  (buffer-substring-no-properties
                   (plist-get prop :contents-begin)
                   (plist-get prop :contents-end)))))
            nil
            'first-match))))))

;;; Graphics
;; From https://github.com/alphapapa/unpackaged.el
(defun org-graph--forward-to-entry-content (&optional unsafe)
  "Skip headline, planning line, and all drawers in current entry.
If UNSAFE is non-nil, assume point is on headline."
  (unless unsafe
    ;; To improve performance in loops (e.g. with `org-map-entries')
    (org-back-to-heading))
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
  (let* ((id (plist-get item :id))
         (name (plist-get item :name))
         (relatives (-list relatives))
         (relatives-list
          (-concat (when (memq 'children relatives)
                     (org-graph-get-children item))
                   (when (memq 'parents relatives)
                     (org-graph-get-parents item)))))
    (propertize
     (format "[[org-node-graph:%s][%s]]"
             id
             (truncate-string-to-width name width nil nil t))
     (intern id) t
     'org-node-graph-id id
     'org-node-graph-name name
     'org-node-graph-relatives
     (-uniq (-map (-lambda ((&plist :id)) id) relatives-list)))))

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

(defcustom org-graph-rendered-properties
  '(
    ("KEYWORDS" . :multi)
    ("SOURCE" . :single)
    ("AUTHOR" . :single)
    ("PUBLISHED" . :single)
    )
  "Properties rendered on the node page.")

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

(defun org-graph-goto-current ()
  "Visit current entry in its org buffer."
  (interactive)
  (org-graph--id-goto (plist-get org-graph-current-entry :id)))

(defun org-graph-revert-buffer ()
  "Revert current node graph buffer."
  (interactive)
  (when org-graph-current-entry
    (org-graph-render-node (plist-get org-graph-current-entry :pom))))

(defun org-graph-render-node (&optional pom)
  (interactive)
  (setq pom (or pom (point-marker)))
  (let* ((current-node (org-graph--make-node pom))
         (buffer (get-buffer-create "*org-node-graph*"))
         (parents (org-graph-get-parents pom))
         (children (org-graph-get-children pom))
         (siblings (org-graph-get-siblings pom))
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
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (org-mode)
      (org-graph-mode 1)
      (variable-pitch-mode -1)

      (insert (format "[[id:%s][Go to current entry]]"
                      (plist-get current-node :id))
              "\n\n")

      (when (> (length parents) 0)
        (--each parents
          (insert (org-graph--render-link it 'children) "    "))
        (insert "\n\n"))

      (insert "- ["
              (org-graph--render-link current-node nil :width 38)
              "]\n")
      (--each siblings
        (insert "- "
                (org-graph--render-link it '(children parents))
                "\n"))
      (insert "\n\n")

      (when (> (length children) 0)
        (goto-char (point-min))
        (goto-char (text-property-any
                    (point-min) (point-max)
                    (intern (plist-get current-node :id)) t))
        (end-of-line)
        (font-lock-ensure (point-min) (point-max))
        (while (< (current-column) 43) (insert " "))
        (insert "-> ")
        (insert-rectangle
         (--map (org-graph--render-link it 'parents) children)))

      (goto-char (point-max))

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

      (put-text-property (point-min) (point-max) 'help-echo 'org-graph-help-echo)
      (goto-char (point-min))

      (read-only-mode 1)
      (setq-local org-graph-current-entry current-node))))

(defun org-graph-cursor-sensor (window _old dir)
  (if (eq dir 'left)
      (ov-clear 'org-node-graph)
    (org-graph-highlight (window-point window))))

(defun org-graph-help-echo (window _object position)
  (if (get-text-property position 'org-node-graph-relatives)
      (with-selected-window window
        (org-graph-highlight position))
    (ov-clear 'org-node-graph))
  (-if-let (name (get-text-property position 'org-node-graph-name))
      name
    ""))

(defun org-graph-highlight (point)
  (ov-clear 'org-node-graph)
  (save-excursion
    (--each (get-text-property point 'org-node-graph-relatives)
      (when-let* ((beg (text-property-any
                        (point-min) (point-max)
                        (intern it) t))
                  (end (next-single-property-change beg (intern it))))
        (ov beg end 'org-node-graph t 'face '(:background "black"))))))


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
    map))

(define-minor-mode org-graph-mode
  "Minor mode for org-graph."
  :lighter " org-graph"
  :keymap org-graph-mode-map)

(provide 'org-graph)
