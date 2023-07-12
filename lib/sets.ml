open Exceptions

type transition = F of string | B of string ;;

module Transition = 
struct 
  type t = transition

  let compare = compare

  let is_forward = function
  | F _ -> true
  | B _ -> false

  let is_backward = function
  | F _ -> false
  | B _ -> true

  let label = function
  | F s
  | B s -> s

  let to_string t = match t with 
  | F s -> s
  | B s -> "!" ^ s
  
  let print t = print_endline (to_string t)
end;; 

module TransitionSet = 
struct
  include Set.Make(Transition)
  
  let forward_subset tt = 
    filter 
    (function F _ -> true | B _ -> false)
    tt

  let backward_subset tt = 
    filter 
    (function F _ -> false | B _ -> true)
    tt
  
  let is_correct tt =
    fold
    (fun t b -> match t with
      | F _ -> b
      | B x -> (mem (F x) tt) && b)
    tt
    true


  let list_to_string t_list = 
    let rec builder list s = match list with 
      | [] -> s 
      | t::l -> builder l (String.concat "," [s; (Transition.to_string t)]) 
    in

    builder t_list ""

  let list_of_sets_to_string s_list = 
    let rec builder list str = match list with 
      | [] -> str
      | s::l -> builder l (String.concat " list: " [str ; (list_to_string (elements s))]) 
    in

    builder s_list ""

  let print s = 
    iter Transition.print s

end;;


type place = 
  | I of int 
  | PrePlace of transition
  | PostPlace of transition 
  | ConfPlace of transition * transition;;     

module Place = 
struct 
  type t = place
  let compare = compare

  let to_string = function
  | I n -> "s" ^ (string_of_int n)
  | PrePlace t -> "(* , " ^ (Transition.to_string t) ^ ")"
  | PostPlace t -> "(" ^ (Transition.to_string t) ^ " , *)"
  | ConfPlace (t, t') -> "({" ^ (Transition.to_string t) ^ ", " ^ (Transition.to_string t') ^ "}, #)"

  let print s = print_endline (to_string s);;
end;; 


module PlaceSet = 
  struct
    include Set.Make(Place)

    let print s = iter Place.print s ;;
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