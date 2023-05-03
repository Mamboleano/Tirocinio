open Tirocinio.Sets
open Tirocinio.Printing
open Tirocinio.Nets

(* All of this code will be added and refactored into the tests. 
   c1 is the net depicted in figure 5(a) of the paper "A distributed
   operational view of Reversible Prime Event Structures", while c2
   is the one depicted in figure 5(b) *)

let s = PlaceSet.of_list [1; 2; 3; 4; 5; 6; 7; 8; 9];;
let t = TransitionSet.of_list [F "a"; F "b"; F "c"; F "d"]
let f = FlowSet.of_list [
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

let m = PlaceSet.of_list [1; 2; 3; 4; 5];;

let c1 : IPT.t = {places = s; transitions = t; flow = f; inhibitors = i; marking = m};;
IPT.print c1;;

let a = TransitionSet.of_list [F "a"; F "b"];;
let preset_a = IPT.preset_of_TransitionSet a c1;;
let postset_a = IPT.postset_of_TransitionSet a c1;;

print_endline "prova preset";;
PlaceSet.print preset_a;;

print_endline "prova postset";;
PlaceSet.print postset_a;;

print_endline "prova enabled";;
print_bool (IPT.is_enabled a c1);;

(* 
print_endline "prova fire";;
IPT.fire_seq a c1;;
IPT.print c1;;
*)

let cr = CN.causality_relation c1;;
let cf = CN.conflict_relation c1;;

CausalityRelation.print cr;;
ConflictRelation.print cf;;
print_endline "Prova pre-causal net";;
print_bool (CN.is_pCN c1);;

print_endline "Prova causal net";;
print_bool (CN.is_CN c1);;


let s' = PlaceSet.of_list [0; 1; 2; 3; 4; 5; 6; 7; 8; 9];;
let f' = FlowSet.of_list [
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
let m' = PlaceSet.of_list [0; 1; 2; 3; 4; 5];;


let c2 : IPT.t = {places = s'; transitions = t; flow = f'; inhibitors = i; marking = m'};;
IPT.print c2;;


print_endline "Prova pre-causal net c2";;
print_bool (CN.is_pCN c2);;

print_endline "Prova causal net c2";;
print_bool (CN.is_CN c2);;