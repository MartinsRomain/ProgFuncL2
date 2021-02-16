type couleur = 
    | Blanc 
    | Noir

type quadtree = 
    | Feuille of couleur
    | Noeud of quadtree * quadtree * quadtree * quadtree;;

(*//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////QUESTION 1//////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////*)


let rec quadtree_full n =
  (*Return full black quadtree which the size is n x n
    val quadtree_full : int -> quadtree = <fun>*)

  if n mod 4 = 0 || n <= 2 then
    match n with
      (*Base case : smallest n = 2. 2x2 = Node(Leaf Black,Leaf Black,Leaf Black,Leaf Black)*)
      | 2 -> Noeud (Feuille Noir, Feuille Noir, Feuille Noir, Feuille Noir)
      (*Recursion : n = 2^k so we divide n by 2. Tree depth = k+1*)
      | a -> Noeud (quadtree_full (n/2),quadtree_full (n/2),quadtree_full (n/2),quadtree_full (n/2))

  (*Throw exception : Unsuitable size*)
  else
    failwith "Size error. Argument must be mutiple of 4 and arg>=2";; 


(*same code as for the previous question except that the quadtree is white*)

let rec quadtree_empty n =
  (*Return full white quadtree which the size is n x n
    val quadtree_full : int -> quadtree = <fun>*)
  if n mod 4 = 0 || n <= 2 then
    match n with
      | 2 -> Noeud (Feuille Blanc, Feuille Blanc, Feuille Blanc, Feuille Blanc)
      | a -> Noeud (quadtree_empty (n/2),quadtree_empty (n/2),quadtree_empty (n/2),quadtree_empty (n/2))
  else
    failwith "Size error. Argument must be mutiple of 4 and arg>=2" ;;


(*/////////////////////////////////////////////////1st question tests///////////////////////////////////////////////////////*)

(*Uncomment the exception tests*)
quadtree_full 8;;
(*quadtree_full 15;;*)

quadtree_empty 16;;
(*quadtree_empty 1;;*)

(*//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////QUESTION 2//////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////*)


let rec inverse a =
  (*This function reverse leaf's colors.
    val inverse : quadtree -> quadtree = <fun>*)

  match a with
    (*Base cases : Return the reversed leaf's color*)
    | Feuille Noir -> Feuille Blanc
    | Feuille Blanc -> Feuille Noir

    (*Recursion : Course each node for each leaf to apply the base case.*)
    | Noeud (w,x,y,z) -> Noeud (inverse w, inverse x, inverse y, inverse z);;

(*/////////////////////////////////////////////////2nd question test///////////////////////////////////////////////////////*)

(*First quadtree_test's initialization*)
let quadtree_test =
  Noeud (
    Noeud (Feuille Noir,Feuille Blanc,Feuille Blanc,Feuille Noir),
    Noeud (Feuille Blanc,Feuille Noir,Feuille Blanc,Feuille Blanc),
    Noeud (Feuille Noir,Feuille Blanc,Feuille Blanc,Feuille Blanc),
    Noeud (Feuille Blanc,Feuille Blanc,Feuille Noir,Feuille Blanc)
  );;

(*Test*)
inverse quadtree_test;;

(*//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////QUESTION 3//////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////*)


let rec rotate quadtree = 
  (*This function turn left a quadtree.
    val rotate : quadtree -> quadtree = <fun>*)

  match quadtree with 
    (*Base cases : Each Leaf go back one index.*)
    | Feuille a -> Feuille a
    | Noeud (Feuille w,Feuille x,Feuille y,Feuille z) -> Noeud(Feuille x,Feuille y,Feuille z,Feuille w)

    (*Recursion : Each node go back one index in quadtree parameter (0,1,2,3) -> (1,2,3,0)*)
    | Noeud (w,x,y,z) -> Noeud (rotate x,rotate y,rotate z,rotate w);;

(*/////////////////////////////////////////////////3rd question test///////////////////////////////////////////////////////*)

rotate quadtree_test;;

(*//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////QUESTION 4//////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////*)


let rec union a b =
  (*This function create new quadtree in accordance with a and b as these rules : Blanc U x = x U Blanc = x ; Noir U Noir = Noir 
    val union : quadtree -> quadtree -> quadtree = <fun>*)

  match a,b with
    (*Throw exception when quadtrees a and b have differents sizes.*)
    | Noeud (_,_,_,_) , Feuille _
    | Feuille _ , Noeud (_,_,_,_) -> failwith "Quadtrees have differents sizes."

    (*Base cases : Apply rules*)
    | a,Feuille Blanc | Feuille Blanc, a -> a
    | Feuille Noir,Feuille Noir -> Feuille Noir

    (*Recursion : Course a and b at same level and apply union*)
    | Noeud (a0,a1,a2,a3),Noeud (b0,b1,b2,b3) -> Noeud (union a0 b0, union a1 b1, union a2 b2, union a3 b3);;

(*/////////////////////////////////////////////////4th question tests///////////////////////////////////////////////////////*)

(*Second and third quadtree_test initializations*)
let i' = rotate quadtree_test;;
let quadtree_unionException = quadtree_empty 8;; (*quadtree_test's size is 16 (16x16)*)

(*Test*)
union quadtree_test i';;

(*///////Exception tests//////*)
(*Uncomment the exception tests*)

(*union quadtree_test quadtree_unionException;;*)

(*//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////QUESTION 5//////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////*)


let rec intersection a b =
  (*This function create new quadtree in accordance with a and b as these rules : Blanc U x = x U Blanc = Blanc ; Noir U Noir = Noir 
    val intersection : quadtree -> quadtree -> quadtree = <fun>*)

  match a,b with
    (*Throw exception when quadtrees a and b have differents sizes.*)
    | Noeud (_,_,_,_) , Feuille _
    | Feuille _ , Noeud (_,_,_,_) -> failwith "Quadtrees have differents sizes."

    (*Base cases : Apply rules*)
    | a,Feuille Blanc | Feuille Blanc, a -> Feuille Blanc
    | Feuille Noir,Feuille Noir -> Feuille Noir

    (*Recursion : Course a and b at same level and apply intersection*)
    | Noeud (a0,a1,a2,a3),Noeud (b0,b1,b2,b3) -> Noeud (intersection a0 b0, intersection a1 b1, intersection a2 b2, intersection a3 b3);;

(*/////////////////////////////////////////////////5th question test///////////////////////////////////////////////////////*)

(*Test*)
intersection quadtree_test i';;

(*///////Exception tests//////*)
(*Uncomment the exception tests*)

(*intersection quadtree_test quadtree_unionException;;*)
(*intersection quadtree_unionException quadtree_test;;*)

(*//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////QUESTION 6//////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////*)


let rec color (x,y) taille quadtree =
  (*This function return the leaf's color in (x,y) coordinates.
    val color : int * int -> int -> quadtree -> couleur = <fun>*)

  (*Throw an exception when x or y exceed the picture size*)
  if x > taille || x <= 0 || y > taille || y <= 0 then
    failwith "Please enter coordinates between 1 and the quadtree's size."
  else
    match quadtree with

      (*Base cases : Return Leaf's color*)
      | Feuille a -> a

      (*Recursion : identifies the quadtree's quarter concerned (a,b,c,d) and call color with this new node.
        With each new call, the quadtree's size is devided by 2, if x>(taille/2), we call color with x-(taille/2). Same for y.*)
      | Noeud (a,b,c,d) ->
          if x <= (taille/2) && y <= (taille/2) then
            color (x,y) (taille/2) d
          else if x <= (taille/2) && y > (taille/2) then
            color (x,(y-(taille/2))) (taille/2) a
          else if x > (taille/2) && y <= (taille/2) then
            color ((x-(taille/2)),y) (taille/2) c
          else
            color ((x-(taille/2)),(y-(taille/2))) (taille/2) b;;

(*/////////////////////////////////////////////////6th question tests///////////////////////////////////////////////////////*)

(*quadtree_test's initialization*)
let quadtree_test_color = Noeud(Noeud(Feuille Noir,Feuille Noir,Feuille Noir,Feuille Noir),
                                Noeud(Feuille Noir,Feuille Blanc,Feuille Noir,Feuille Noir),
                                Noeud(Feuille Noir,Feuille Noir,Feuille Noir,Feuille Noir),
                                Noeud(Feuille Noir,Feuille Noir,Feuille Noir,Feuille Noir));;

color (4,4) 4 quadtree_test_color;;

(*///////Exception tests//////*)
(*Uncomment the exception tests*)

(* For x=0 or y=0*)
(*color (0,3) 4 quadtree_test*)
(*color (1,0) 4 quadtree_test*)

(* For x=taille or y=taille *)
(*color (2,5) 4 quadtree_test*)
(*color (7,1) 4 quadtree_test*)

(*//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////QUESTION 7//////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////*)


let rec modify quadtree current_x current_y fonction taille =
  (*Modify Leaf color after testing coordinates (in fonction)
    int -> int -> (int -> int -> couleur -> couleur) -> int -> quadtree = <fun>*)
  match quadtree with 
    (*Base case : We apply fonction with current x,y,color to test it in the fonction in argument*)
    | Feuille e -> Feuille (fonction current_x current_y e)
    | Noeud (a,b,c,d)
      (*Course the tree by increasing x,y as it was the picture reconsitution in an orthonormal coordinate system*)
      ->  Noeud (modify a current_x (current_y+taille/2) fonction (taille/2), 
                 modify b (current_x+taille/2) (current_y+taille/2) fonction (taille/2),
                 modify c (current_x+taille/2) current_y fonction (taille/2),
                 modify d current_x current_y fonction (taille/2));;

(*/////////////////////////////////////////////////7th question tests///////////////////////////////////////////////////////*)


(*quadtree_test's initialization*)
let quadtree_test_modify = quadtree_full 4;;

(*Test*)
modify quadtree_test_modify 1 1 (fun x y couleur_ini -> if x=3 && y=1 && couleur_ini = Noir then Blanc else couleur_ini) 4;;


(*//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////QUESTION 8//////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////*)


let rec optimise quadtree =
  (*This function return an optimal quadtree (Noeud(Feuille Noir,Feuille Noir,Feuille Noir,Feuille Noir) = Feuille Noir)
    val optimise : quadtree -> quadtree = <fun>*)
  match quadtree with 
    (*Base cases : Return leaf
      Leaves are joined as in a function's description*)
    | Feuille e -> Feuille e
    | Noeud (Feuille Noir,Feuille Noir,Feuille Noir,Feuille Noir) -> Feuille Noir
    | Noeud (Feuille Blanc,Feuille Blanc,Feuille Blanc,Feuille Blanc) -> Feuille Blanc
    (*Recursion : Visit all of leaves*)
    | Noeud (a,b,c,d) -> 
        if Noeud (optimise a, optimise b, optimise c, optimise d) = Noeud (Feuille Noir, Feuille Noir, Feuille Noir, Feuille Noir) then
          Feuille Noir
        else if Noeud (optimise a, optimise b, optimise c, optimise d) = Noeud (Feuille Blanc, Feuille Blanc, Feuille Blanc, Feuille Blanc) then
          Feuille Blanc
        else 
          (* We test it after returning from function to verify if new node is monochrome*)
          Noeud (optimise a, optimise b, optimise c, optimise d);;

(*/////////////////////////////////////////////////8th question tests///////////////////////////////////////////////////////*)

(*New quadtree_test's initializations*)
let quadtree_test_optimise = Noeud(
                               Noeud(Feuille Noir, Feuille Noir, Feuille Noir, Feuille Noir),
                               Noeud(Feuille Blanc, Feuille Noir, Feuille Noir, Feuille Noir),
                               Noeud(Feuille Noir, Feuille Noir, Feuille Blanc, Feuille Noir),
                               Noeud(Feuille Noir, Feuille Noir, Feuille Noir, Feuille Noir));;

let quadtree_test_empty_optimise = quadtree_empty 8;;

(*Tests*)
optimise quadtree_test_optimise;;
optimise quadtree_test_empty_optimise;;

(*//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////QUESTION 9//////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////*)


(*Type's initialization*)
type bit = Zero | Un;;


let rec quadtree_to_list quadtree =
  (*Converts quadtree to bit list
    val quadtree_to_list : quadtree -> bit list = <fun>*)
  match quadtree with
    (*base cases : We use bit type and homogenize them*)
    | Feuille Noir -> Zero::Un::[]
    | Feuille Blanc -> Zero::Zero::[]
    (**)
    | Noeud (a,b,c,d) -> Un::[] @ quadtree_to_list a @ quadtree_to_list b @ quadtree_to_list c @ quadtree_to_list d;;

(*/////////////////////////////////////////////////9th question test///////////////////////////////////////////////////////*)

quadtree_to_list quadtree_test

(*//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////QUESTION 10//////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////*)


(*Attention : This function doesn't return the expected result*)


let rec list_to_quadtree liste =
  match liste with
      [] -> failwith"Error"
    | Zero::Zero::t -> Feuille Blanc 
    | Zero::Un::t -> Feuille Noir
    | Un::first -> match first with
      | h::i::second ->
          match second with
            | h::i::third ->
                match third with
                  | h::i::fourth ->
                      Noeud(list_to_quadtree first, list_to_quadtree second, list_to_quadtree third, list_to_quadtree fourth);;

(*/////////////////////////////////////////////////10th question test///////////////////////////////////////////////////////*)

(*list_test's initialization*)
let list_test =  quadtree_to_list quadtree_test;;
(*Test*)
list_to_quadtree list_test;;


























