(defun org-graph-target (prompt)
  (let ((org-refile-target-verify-function nil))
    (org-refile-get-location prompt)))

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
    (-map
     (lambda (p)
       (org-with-point-at p
         (list :pom p
               :id (org-id-get-create)
               :name (org-get-heading 'no-tags 'no-todo))))
     (if parent-from-tree
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
    (let ((parents (org-entry-get-multivalued-property nil "GRAPH_CHILDREN")))
      (-map (lambda (entry) (org-id-find entry 'marker)) parents))))

(defun org-graph-get-children (&optional pom)
  "Return all children at POM.

If POM is a list, first extract the :pom property and use that."
  (setq pom (or (and (listp pom)
                     (plist-get pom :pom))
                pom
                (point-marker)))
  (let ((children-from-property (org-graph--get-children-from-property pom))
        (children-from-tree (org-graph--get-children-from-tree pom)))
    (-map
     (lambda (p)
       (org-with-point-at p
         (list :pom p
               :id (org-id-get-create)
               :name (org-get-heading 'no-tags 'no-todo))))
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
