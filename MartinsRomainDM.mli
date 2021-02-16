type couleur = Blanc | Noir
type quadtree =
    Feuille of couleur
  | Noeud of quadtree * quadtree * quadtree * quadtree
val quadtree_full : int -> quadtree
val quadtree_empty : int -> quadtree
val inverse : quadtree -> quadtree
val quadtree_test : quadtree
val rotate : quadtree -> quadtree
val union : quadtree -> quadtree -> quadtree
val i' : quadtree
val quadtree_unionException : quadtree
val intersection : quadtree -> quadtree -> quadtree
val color : int * int -> int -> quadtree -> couleur
val quadtree_test_color : quadtree
val modify :
  quadtree ->
  int -> int -> (int -> int -> couleur -> couleur) -> int -> quadtree
val quadtree_test_modify : quadtree
val optimise : quadtree -> quadtree
val quadtree_test_optimise : quadtree
val quadtree_test_empty_optimise : quadtree
type bit = Zero | Un
val quadtree_to_list : quadtree -> bit list
val list_to_quadtree : bit list -> quadtree
val list_test : bit list
