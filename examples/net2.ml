open Tirocinio.Sets
open Tirocinio.Nets
(* c2 is the net depicted in figure 5(b) of the paper "A distributed
   operational view of Reversible Prime Event Structures" *)

let s = PlaceSet.of_list [0; 1; 2; 3; 4; 5; 6; 7; 8; 9];;

let t = TransitionSet.of_list [F "a"; F "b"; F "c"; F "d"];;

let f = FlowSet.of_list [
  {source = S(0); target = T(F "a")};
  {source = S(0); target = T(F "c")};
  {source = S(1); target = T(F "a")};
  {source = S(2); target = T(F "a")};
  {source = S(2); target = T(F "b")};
  {source = S(3); target = T(F "b")};
  {source = S(4); target = T(F "c")};
  {source = S(5); target = T(F "d")}; 

  {source = T(F "a"); target = S(6)};
  {source = T(F "b"); target = S(7)};
  {source = T(F "c"); target = S(8)};
  {source = T(F "d"); target = S(9)};
  ]
;;

let i = InhibitorSet.of_list [{source = S(3); target = T(F "c")}];;
let m = PlaceSet.of_list [0; 1; 2; 3; 4; 5];;

let c2 : IPT.t = {places = s; transitions = t; flow = f; inhibitors = i; marking = m};;