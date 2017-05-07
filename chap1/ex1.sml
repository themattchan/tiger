structure Tree = struct
  type key = string
  datatype 'a tree = LEAF | TREE of tree * 'a * tree

  fun insert (key, LEAF)       = TREE(LEAF, key, LEAF)
    | insert (key, TREE(l,k,r) =
                   if key < k then TREE (insert (key, l), k,r) else
                   if key > k then TREE (l,k, insert (key, r)) else
                   TREE(l,k,r)


  fun member (key, LEAF) = false
    | member (key, TREE(l,k,r)) =
      if key < k then member (key, l) else
      if key > k then member (key, r) else
      true
end

signature KVTREE = sig
  type key = string
  datatype 'a tree = LEAF | TREE of tree * (key * 'a) * tree
  val insert : key * 'a * 'a tree -> 'a tree
  val lookup : key * 'a tree -> 'a option
end;

structure KVTree : KVTREE =
struct

  fun insert (key, value, LEAF)           = TREE(LEAF, (key,value),LEAF)
    | insert (key, value, TREE(l,(k,v),r) =
                   if key < k then TREE (insert (key, value, l), k,r) else
                   if key > k then TREE (l,k, insert (key, value, r)) else
                   TREE(l,(k,v),r);


  fun lookup (key, LEAF) = NONE
    | lookup (key, TREE(l,(k,v),r)) =
      if key < k then member (key, l) else
      if key > k then member (key, r) else
      SOME v

end;
