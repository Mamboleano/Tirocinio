open Types

(* Function to print several elements of Petri Nets*)
let print_place = function
  | S(n) -> print_endline ("s" ^ (string_of_int n))

let print_transition = function
  | T(str) -> print_endline str
;;

let print_PlaceSet s =
  PlaceSet.iter print_place s;;

let print_TransitionSet s =
  TransitionSet.iter print_transition s;;

