open Types

let print_bool = function 
  | true -> print_endline "True"
  | false -> print_endline "False"
;;

(* Function to print several elements of Petri Nets*)
let string_of_place = function
  | S(n) -> "s" ^ (string_of_int n)
;;

let print_place s = print_endline (string_of_place s);;

let string_of_transition = function
  | T(str) -> str
;;

let print_transition t = print_endline (string_of_transition t);;

let string_of_arc = function 
  | PreArc(s, t) -> string_of_place s ^ " -> " ^ string_of_transition t
  | PostArc(t, s) -> string_of_transition t ^ " -> " ^ string_of_place s
;;

let print_arc x = print_endline (string_of_arc x);;

let string_of_inhibitor = function 
  | InhibArc(s, t) -> string_of_place s ^ " -o " ^ string_of_transition t
;;

let print_inhibitor i = print_endline (string_of_inhibitor i);;

let print_PlaceSet s =
  PlaceSet.iter print_place s ;;

let print_TransitionSet t =
  TransitionSet.iter print_transition t;;

let print_FlowSet f = 
  FlowSet.iter print_arc f ;;

let print_InhibitorSet i = 
  InhibitorSet.iter print_inhibitor i;;

let print_ipt (s, t, f, i, m) =
  print_endline "\nPlaces:";
  print_PlaceSet s;

  print_endline "\nTransitions:";
  print_TransitionSet t;

  print_endline "\nFlow Relation:";
  print_FlowSet f;

  print_endline "\nInhibitor arcs:";
  print_InhibitorSet i;

  print_endline "\nMarking:";
  print_PlaceSet m;
;;
