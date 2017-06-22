;;; Parents

(defun org-graph--get-parents-from-tree (&optional pom)
  "Get all parents of the headline at POM.

If any of the parents has the property GRAPH_PARENT_SKIP this
parent is not included in the parents but its parents *are*
traversed.

If any of the parents (including this entry) has the property
GRAPH_PARENT_ROOT all the parents above this headline will be
ignored.  This root parent is included, so if you want to skip it
as well give it GRAPH_PARENT_SKIP property as well.

Return list of markers pointing to the parent entries."
  (org-with-point-at pom
    (let ((org-agenda-skip-function-global nil)
          re)
      (catch 'done
        (while (org-up-heading-safe)
          (unless (org-entry-properties nil "GRAPH_PARENT_SKIP")
            (push (point-marker) re))
          (when (org-entry-properties nil "GRAPH_PARENT_ROOT")
            (throw 'done t))))
      (nreverse re))))

(defun org-graph--get-parents-from-property (&optional pom)
  "Get all parents specified in the GRAPH_PARENTS property at POM.

Return list of markers pointing to the parent entries."
  (org-with-point-at pom
    (let ((parents (org-entry-get-multivalued-property nil "GRAPH_PARENTS")))
      (-map (lambda (entry) (org-id-find entry 'marker)) parents))))

(defun org-graph-get-parents (&optional pom)
  "Return all parents at POM."
  (let ((parents-from-property (org-graph--get-parents-from-property pom))
        (parents-from-tree (org-graph--get-parents-from-tree pom)))
    (-map
     (lambda (p)
       (org-with-point-at p
         (list :pom p :name (org-get-heading 'no-tags 'no-todo))))
     (-concat parents-from-tree parents-from-property))))

(defun org-graph-add-parent (&optional pom)
  (interactive)
  (org-with-point-at pom
    (-when-let (parent (-last-item (org-refile-get-location "Parent: ")))
      (let ((my-id (org-id-get-create))
            (parent-id (org-with-point-at parent (org-id-get-create))))
        (org-entry-add-to-multivalued-property parent "GRAPH_CHILDREN" my-id)
        (org-entry-add-to-multivalued-property pom "GRAPH_PARENTS" parent-id)))))

;;; Children
(defun org-graph--get-children-from-tree (&optional pom)
  "Get all children of the headline at POM.

If any of the children has the property GRAPH_CHILD_SKIP this
child is not included in the children but its children *are*
traversed.

If any of the children has the property GRAPH_CHILD_LEAF no
children of that headline will be included.  This leaf headline
is included, so if you want to skip it as well give it the
GRAPH_CHILD_SKIP property.

Return list of markers pointing to the child entries."
  (org-with-point-at pom
    (org-back-to-heading t)
    (setq pom (point-marker))
    (let* ((org-agenda-skip-function-global nil)
           (re nil)
           (children
            (org-map-entries
             'point-marker t 'tree
             (lambda ()
               (cond
                ;; do not skip if we started from this node and the
                ;; leaf property is "self"
                ((and (equal "self" (org-entry-get (point) "GRAPH_CHILD_LEAF"))
                      (equal (marker-position pom) (point)))
                 nil)
                ((org-entry-properties nil "GRAPH_CHILD_LEAF")
                 ;; we still want to include this one if it doesn't
                 ;; have skip on it
                 (progn
                   (unless (org-entry-properties nil "GRAPH_CHILD_SKIP")
                     (push (point-marker) re))
                   (save-excursion (org-end-of-subtree t))))
                (t
                 (when (org-entry-properties nil "GRAPH_CHILD_SKIP")
                   (save-excursion
                     (outline-next-heading)
                     (point)))))))))
      (let ((re (-sort '< (-concat re children))))
        ;; pop the first entry if it is the current one, we don't want
        ;; to duplicate it
        (when (= (car re) (save-excursion (org-back-to-heading t) (point)))
          (pop re))
        re))))

(defun org-graph--get-children-from-property (&optional pom)
  "Get all children specified in the GRAPH_CHILDREN property at POM.

Return list of markers pointing to the child entries."
  (org-with-point-at pom
    (let ((parents (org-entry-get-multivalued-property nil "GRAPH_CHILDREN")))
      (-map (lambda (entry) (org-id-find entry 'marker)) parents))))

(defun org-graph-get-children (&optional pom)
  "Return all children at POM."
  (let ((children-from-property (org-graph--get-children-from-property pom))
        (children-from-tree (org-graph--get-children-from-tree pom)))
    (-map
     (lambda (p)
       (org-with-point-at p
         (list :pom p :name (org-get-heading 'no-tags 'no-todo))))
     (-concat children-from-tree children-from-property))))

(defun org-graph-add-child (&optional pom)
  (interactive)
  (org-with-point-at pom
    (-when-let (child (-last-item (org-refile-get-location "Child: ")))
      (let ((my-id (org-id-get-create))
            (child-id (org-with-point-at child (org-id-get-create))))
        (org-entry-add-to-multivalued-property child "GRAPH_PARENTS" my-id)
        (org-entry-add-to-multivalued-property pom "GRAPH_CHILDREN" child-id)))))
