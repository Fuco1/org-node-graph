;; -*- lexical-binding: t -*-

(require 'buttercup)

(require 'org-graph)

(defmacro with-org-buffer (content &rest body)
  (declare (indent 1))
  `(let ((org-id-track-globally nil)
         (org-id-locations-file nil))
     (with-temp-buffer
       (insert ,content)
       (goto-char (point-min))
       (org-mode)
       ,@body)))

(defmacro with-org-buffer-from-file (file &rest body)
  (declare (indent 1))
  `(let ((org-id-track-globally nil)
         (org-id-locations-file nil))
     (with-temp-buffer
       (insert-file-contents ,file)
       (goto-char (point-min))
       (org-mode)
       ,@body)))

(describe "Working with relations"

  (describe "in hierarchy"

    (describe "of parents"

      (it "should recognize the hierarchy parent as a graph parent"
        (with-org-buffer-from-file "tests/hierarchy-parent.org"
          (search-forward "** Sub 1.1")
          (let ((parents (org-graph-get-parents)))
            (expect (length parents) :to-be 1)
            (expect (plist-get (car parents) :name) :to-equal "Top 1")
            (expect (plist-get (car parents) :id) :to-equal "70fe339d-9fc9-4850-988a-f7bfe52a9897"))))

      (it "should skip the hierarchy parent marked with GRAPH_PARENT_SKIP"
        (with-org-buffer-from-file "tests/hierarchy-parent.org"
          (search-forward "*** Sub 1.2.1")
          (let ((parents (org-graph-get-parents)))
            (expect (length parents) :to-be 1)
            (expect (plist-get (car parents) :name) :to-equal "Top 1")
            (expect (plist-get (car parents) :id) :to-equal "70fe339d-9fc9-4850-988a-f7bfe52a9897"))))

      (it "should use a headline marked with GRAPH_PARENT_ROOT as parent if it is not marked to be skipped"
        (with-org-buffer-from-file "tests/hierarchy-parent.org"
          (search-forward "*** Sub 1.3.1")
          (let ((parents (org-graph-get-parents)))
            (expect (length parents) :to-be 1)
            (expect (plist-get (car parents) :name) :to-equal "Sub 1.3")
            (expect (plist-get (car parents) :id) :to-equal "e5104d09-df30-4bab-bd91-a4b5075fde44"))))

      (it "should not use a headline marked with GRAPH_PARENT_ROOT as parent if it is also marked to be skipped"
        (with-org-buffer-from-file "tests/hierarchy-parent.org"
          (search-forward "*** Sub 1.4.1")
          (let ((parents (org-graph-get-parents)))
            (expect (length parents) :to-be 0))))

      (it "should skip over nodes with GRAPH_PARENT_SKIP and stop at a GRAPH_PARENT_ROOT node"
        (with-org-buffer-from-file "tests/hierarchy-parent.org"
          (search-forward "**** Sub 1.5.1.1")
          (let ((parents (org-graph-get-parents)))
            (expect (length parents) :to-be 1)
            (expect (plist-get (car parents) :name) :to-equal "Sub 1.5")
            (expect (plist-get (car parents) :id) :to-equal "95d910a7-3a13-4993-9d60-637eef6ff46e"))))

      (it "should not return any hierarchy parent at a node marked with GRAPH_PARENT_ROOT"
        (with-org-buffer-from-file "tests/hierarchy-parent.org"
          (search-forward "** Sub 1.6")
          (let ((parents (org-graph-get-parents)))
            (expect (length parents) :to-be 0))))

      (it "should stop the search at a GRAPH_PARENT_ROOT node"
        (with-org-buffer-from-file "tests/hierarchy-parent.org"
          (search-forward "*** Sub 1.7.1")
          (let ((parents (org-graph-get-parents)))
            (expect (length parents) :to-be 0)))))


    (describe "of children"

      (it "should recognize all children of one level deeper as graph children"
        (with-org-buffer-from-file "tests/hierarchy-children.org"
          (search-forward "* Top 1")
          (let ((children (org-graph-get-children)))
            (expect (length children) :to-be 2)
            (expect (plist-get (car children) :name) :to-equal "Sub 1.1")
            (expect (plist-get (car children) :id) :to-equal "f37496aa-9240-4d69-a44f-000b3a59b40b")

            (expect (plist-get (cadr children) :name) :to-equal "Sub 1.2")
            (expect (plist-get (cadr children) :id) :to-equal "c3e4b389-6496-438f-88eb-1eb8126e5bc5"))))

      (it "should ignore children marked with GRAPH_CHILD_SKIP"
        (with-org-buffer-from-file "tests/hierarchy-children.org"
          (search-forward "* Top 2")
          (let ((children (org-graph-get-children)))
            (expect (length children) :to-be 1)
            (expect (plist-get (car children) :name) :to-equal "Sub 2.2")
            (expect (plist-get (car children) :id) :to-equal "ca1173bd-4cf9-4554-9317-7aefce20eb6d"))))

      (it "should recognize children deeper than one level if they immediately follow the parent headline as graph children"
        (with-org-buffer-from-file "tests/hierarchy-children.org"
          (search-forward "* Top 3")
          (let ((children (org-graph-get-children)))
            (expect (length children) :to-be 2)
            (expect (plist-get (car children) :name) :to-equal "Sub 3.1.1")
            (expect (plist-get (car children) :id) :to-equal "33c46f71-1a30-4a84-99f0-b98c8e7e21ab")

            (expect (plist-get (cadr children) :name) :to-equal "Sub 3.2")
            (expect (plist-get (cadr children) :id) :to-equal "799ff262-e19f-4d5b-b130-1f80b8033e45"))))

      (it "should not recognize children deeper than one level if they are nested under a headline which is between it and the tested node"
        (with-org-buffer-from-file "tests/hierarchy-children.org"
          (search-forward "* Top 4")
          (let ((children (org-graph-get-children)))
            (expect (length children) :to-be 1)
            (expect (plist-get (car children) :name) :to-equal "Sub 4.1")
            (expect (plist-get (car children) :id) :to-equal "d04c7df3-9fbf-4f2c-82b5-5a71686d45e0"))))

      (it "should recognize children marked with GRAPH_CHILD_LEAF as graph children"
        (with-org-buffer-from-file "tests/hierarchy-children.org"
          (search-forward "* Top 5")
          (let ((children (org-graph-get-children)))
            (expect (length children) :to-be 1)
            (expect (plist-get (car children) :name) :to-equal "Sub 5.1")
            (expect (plist-get (car children) :id) :to-equal "bf73f2ad-6950-4a57-b543-d0638fb22baf"))))

      (it "should return no hierarchy children for nodes marked with GRAPH_CHILD_LEAF"
        (with-org-buffer-from-file "tests/hierarchy-children.org"
          (search-forward "* Top 6")
          (let ((children (org-graph-get-children)))
            (expect (length children) :to-be 0)))))))
