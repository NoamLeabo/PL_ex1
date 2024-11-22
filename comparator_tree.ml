let rec comparator_tree tree e cmp = match tree with
        | Empty -> Node(e, Empty, Empty)
        | Node(root, l, r) -> if cmp root e then
                                Node(root, comparator_tree l e cmp, r)
                              else
                                Node(root, l, comparator_tree r e cmp);;
