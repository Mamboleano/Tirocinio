open Tirocinio.Sets
open Tirocinio.Nets

(* c1 is the net depicted in figure 7 of the paper "A distributed
   operational view of Reversible Prime Event Structures" *)

let s = PlaceSet.of_list [I(0); I(1); I(2); I(3); I(4); I(5); I(6); I(7); I(8); I(9)];;
let t = TransitionSet.of_list [F "a"; F "b"; F "c"; F "d"; B "b"; B "c"]
let f = FlowSet.of_list [
  {source = S(I(0)); target = T(F "a")};
  {source = S(I(0)); target = T(F "c")};
  {source = S(I(1)); target = T(F "a")};
  {source = S(I(2)); target = T(F "a")};
  {source = S(I(2)); target = T(F "b")};
  {source = S(I(3)); target = T(F "b")};
  {source = S(I(4)); target = T(F "c")};
  {source = S(I(5)); target = T(F "d")}; 

  {source = S(I(7)); target = T(B "b")};
  {source = S(I(8)); target = T(B "c")};

  {source = T(F "a"); target = S(I(6))};
  {source = T(F "b"); target = S(I(7))};
  {source = T(F "c"); target = S(I(8))};
  {source = T(F "d"); target = S(I(9))};

  {source = T(B "b"); target = S(I(2))};
  {source = T(B "b"); target = S(I(3))};
  {source = T(B "c"); target = S(I(0))};
  {source = T(B "c"); target = S(I(4))};
  ]
;;


let i = InhibitorSet.of_list [
  {source = S(I(3)); target = T(F "c")};
  {source = S(I(3)); target = T(B "b")};
  {source = S(I(4)); target = T(B "c")};
  {source = S(I(5)); target = T(B "c")};
  {source = S(I(8)); target = T(B "b")};
  
];;

let m = PlaceSet.of_list [I(0); I(1); I(2); I(3); I(4); I(5)];;

let c3 : ReversibleCN.t = {places = s; transitions = t; flow = f; inhibitors = i; marking = m};;