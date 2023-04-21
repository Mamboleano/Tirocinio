(* Here we define a type for the places S, we will consider the as integers preceeded with a label S.
   In order to represent them as Sets, we define a module OrderedPlaces to give an order between places, 
   after that, we can define the module PlaceSet using the already existing Set module*)
type place = S of int;;

module OrderedPlaces = 
struct 
  type t = place
  let compare x y = match (x,y) with
    | S(n), S(m) -> if n = m then 0 else if n<m then -1 else 1
end;; 

module PlaceSet = Set.Make(OrderedPlaces);; 

(* Here, we do the same for transitions T: we define them as strings preceeded with a label T and create the relative modules*)
type transition = T of string;;

module OrderedTransitions = 
struct 
  type t = transition
  let compare x y = match (x,y) with
    | T(s1), T(s2) -> String.compare s1 s2
end;; 

module TransitionSet = Set.Make(OrderedTransitions);;



type arc = PreArc of place * transition | PostArc of transition * place | InibArc of place * transition

type marking = Mark of PlaceSet.t;;


 