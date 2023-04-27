open Exceptions

(* Here we define a type for the places S, we will consider the as integers preceeded with a label S.
   In order to represent them as Sets, we define a module Place to give an order between places, 
   after that, we can define the module PlaceSet using the already existing Set module*)

type place = int;;     

module Place = 
struct 
  type t = place
  let compare = Int.compare

  let to_string = function
  | n -> "s" ^ (string_of_int n)

  let print s = print_endline (to_string s);;
end;; 


module PlaceSet = 
  struct
    include Set.Make(Place)

    let print s = iter Place.print s ;;
end;;

(* Here, we do the same for transitions T: we define them as strings preceeded with a label T and create the relative modules*)
type transition = string;;

module Transition = 
struct 
  type t = transition

  let compare = String.compare

  let to_string s = s
  
  let print t = print_endline (to_string t)
end;; 

module TransitionSet = 
struct
  include Set.Make(Transition)
  
  let print s = iter Transition.print s

end;;


type node = S of place | T of transition;;

module Node = 
struct
  type t = node

  let compare = compare

  let is_place = function S _ -> true | T _ -> false
  let is_transition = function S _ -> false | T _ -> true

  let to_string = function 
    | S(n) -> Place.to_string n 
    | T(s) -> Transition.to_string s

  let print nd = print_endline (to_string nd)
end;;

type arc = {source : node; target : node};;

(* This represents the Flow relation, it can be seen as the union of Presets and Postsets of all transitions and places of the net, 
   a step of the flow relation can consist in place -> transition or transition -> place*)

(* Here we give an order to the flow relation, we put the palce -> transition type of steps before the transition -> place ones*)
module Flow =
struct
  type t = arc

  let is_correct = function 
    | {source = T(_); target = S(_)}
    | {source = S(_); target = T(_)} -> true 
    | _ -> false

  let compare = compare

  let to_string f = 
    if is_correct f then
      Node.to_string f.source ^ " -> " ^ Node.to_string f.target
    else
      raise IllegalFlow

  let print x = print_endline (to_string x)
end;;

module FlowSet = 
struct 
  include Set.Make(Flow);;

  let print s = iter Flow.print s
end;;
(* Here we define the set of inibitor arcs of a causal net such that InibArc(s, t) means that the place s inhibits the transition t *)

module InhibArc =
struct
  type t = arc

  let is_correct = function 
    | {source = S(_); target = T(_)} -> true 
    | _ -> false


  let compare = compare
  
  let to_string i =  
    if is_correct i then  
      Node.to_string i.source ^ " -o " ^ Node.to_string i.target
    
  else
    raise IllegalArc
  
  let print i = print_endline (to_string i)
end;;

module InhibitorSet = 
struct
  include Set.Make(InhibArc);;
  
  let print s = iter InhibArc.print s
end;;