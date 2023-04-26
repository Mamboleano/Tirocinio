open Exceptions

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


(* This represents the Flow relation, it can be seen as the union of Presets and Postsets of all transitions and places of the net, 
   a step of the flow relation can consist in place -> transition or transition -> place*)
type flowArc = PreArc of place * transition | PostArc of transition * place;;


(* The following two are just two auxiliary functions to help sort the flow relation*)
let comparePreArc f1 f2 = match (f1, f2) with
  | PreArc(s, t), PreArc(s', t') -> 
    if OrderedPlaces.compare s s' = 0 then OrderedTransitions.compare t t'
    else OrderedPlaces.compare s s'
  | _ -> raise (TypeError "They both need to be Pre Arcs")
;;

let comparePostArc f1 f2 = match (f1, f2) with
  | PostArc(t, s), PostArc(t', s') -> 
    if OrderedTransitions.compare t t' = 0 then OrderedPlaces.compare s s' 
    else OrderedTransitions.compare t t'
  | _ -> raise (TypeError "They both need to be Post Arcs")
;;

(* Here we give an order to the flow relation, we put the palce -> transition type of steps before the transition -> place ones*)
module OrderedFlowArcs =
struct 
  type t = flowArc
  let compare x y = match (x,y) with
    | PreArc(_), PostArc(_) -> -1
    | PostArc(_), PreArc(_) -> 1
    | PreArc(s, t), PreArc(s', t') -> comparePreArc (PreArc(s, t)) (PreArc(s', t'))
    | PostArc(t, s), PostArc(t', s') -> comparePostArc (PostArc(t, s)) (PostArc(t', s'))
end;; 

module FlowSet = Set.Make(OrderedFlowArcs);;

(* Here we define the set of inibitor arcs of a causal net such that InibArc(s, t) means that the place s inhibits the transition t *)
type inibArc = InhibArc of place * transition;;

module OrderedInibArcs =
struct
  type t = inibArc

  let compare x y = match (x,y) with
  | InhibArc(s, t), InhibArc(s', t') -> 
      if OrderedPlaces.compare s s' = 0 then OrderedTransitions.compare t t'
      else OrderedPlaces.compare s s'
end;;

module InhibitorSet = Set.Make(OrderedInibArcs);;

(* This represent a mark*)
type marking = Mark of PlaceSet.t;;

(* This represents a Petri Net with inhibitor arcs *)
type ipt = PlaceSet.t * TransitionSet.t * FlowSet.t * InhibitorSet.t * marking ;;
 