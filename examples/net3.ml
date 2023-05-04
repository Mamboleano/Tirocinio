open Tirocinio.Sets
open Tirocinio.Nets

(* c1 is the net depicted in figure 7 of the paper "A distributed
   operational view of Reversible Prime Event Structures" *)

let s = PlaceSet.of_list [0; 1; 2; 3; 4; 5; 6; 7; 8; 9];;
let t = TransitionSet.of_list [F "a"; F "b"; F "c"; F "d"; B "b"; B "c"]
let f = FlowSet.of_list [
  {source = S(0); target = T(F "a")};
  {source = S(0); target = T(F "c")};
  {source = S(1); target = T(F "a")};
  {source = S(2); target = T(F "a")};
  {source = S(2); target = T(F "b")};
  {source = S(3); target = T(F "b")};
  {source = S(4); target = T(F "c")};
  {source = S(5); target = T(F "d")}; 

  {source = S(7); target = T(B "b")};
  {source = S(8); target = T(B "c")};

  {source = T(F "a"); target = S(6)};
  {source = T(F "b"); target = S(7)};
  {source = T(F "c"); target = S(8)};
  {source = T(F "d"); target = S(9)};

  {source = T(B "b"); target = S(2)};
  {source = T(B "b"); target = S(3)};
  {source = T(B "c"); target = S(0)};
  {source = T(B "c"); target = S(4)};
  ]
;;


let i = InhibitorSet.of_list [
  {source = S(3); target = T(F "c")};
  {source = S(3); target = T(B "b")};
  {source = S(4); target = T(B "c")};
  {source = S(5); target = T(B "c")};
  {source = S(8); target = T(B "b")};
  
  ];;

let m = PlaceSet.of_list [0; 1; 2; 3; 4; 5];;

let c3 : IPT.t = {places = s; transitions = t; flow = f; inhibitors = i; marking = m};;