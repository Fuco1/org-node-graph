# Relations

Relations are maintained through the outline hierarchy and special properties on the entries so we can model arbitrary graphs instead of only trees.  This means you can define parents or children completely outside the hierarchy or even in different files.

Relations are kept in sync bidirectionally so please only use the API to maintain them otherwise things might get lost.  Because the relations are bidirectional the graph traversal and querying is extremly fast.

## Parents

Parents are defined by the `GRAPH_PARENTS` property as list of IDs and implicitly through the org outline hierarchy: all headlines above this one are this entry's parents.

You can optionally skip parents defined by hierarchy if you give them non-nil property `GRAPH_PARENT_SKIP`.  This will skip the parent with the property but will continue the traversal further up.

You can optionally skip all parents above the current one by giving it non-nil property `GRAPH_PARENT_ROOT`.  This will include this parent but will not traverse any further.  If you do not want to include the root give it the `GRAPH_PARENT_SKIP` property as well.

## Children

Children are defined by the `GRAPH_CHILDREN` property as list of IDs and implicitly through the org outline hierarchy: the entire subtree (excluding current entry) are this entry's children.

You can optionally skip children defined by hierarchy if you give them non-nil property `GRAPH_CHILD_SKIP`.  This will skip the child with the property but will continue the traversal further down.

You can optionally skip all children below the current child by giving it non-nil property `GRAPH_CHILD_LEAF`.  This will include this child but will not traverse any further.  If you do not want to include the leaf give it the `GRAPH_CHILD_SKIP` property as well.

# API

- [x] `org-graph-get-parents` to get parents of current entry.
- [x] `org-graph-get-children` to get children of current entry.
- [x] `org-graph-add-parent` to add a parent to the current entry.
  - [ ] check if the parent is defined by hierarchy and do not add the ID
- [x] `org-graph-add-child` to add a child to the current entry.
  - [ ] check if the child is defined by hierarchy and do not add the ID
- [ ] `org-graph-remove-parent` to remove a parent from the current entry.
- [ ] `org-graph-remove-child` to remove a child from the current entry.

It would be also nice to add some way to store *link* properties (that is, label/annotate the edges of the graph).  We might use some simple property format like `:GRAPH_EDGE: id <properties>`.  This can only be stored on e.g. parent (or child) because we can get to the other end of the relation relatively simply by using the traversal properties.
