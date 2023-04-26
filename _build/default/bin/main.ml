open Tirocinio.Types
open Tirocinio.Printing
open Tirocinio.Utils
open Tirocinio.Proprieties

let s = PlaceSet.of_list [S(1); S(2); S(3); S(4); S(5); S(6); S(7); S(8);S(9)];;
let t = TransitionSet.of_list [T("a"); T("b"); T("c"); T("d")]
let f = FlowSet.of_list [
  PreArc(S(1), T("a")); 
  PreArc(S(2), T("a"));
  PreArc(S(2), T("b"));
  PreArc(S(3), T("b"));
  PreArc(S(4), T("c"));
  PreArc(S(5), T("d"));
  
  PostArc(T("a"), S(6));
  PostArc(T("b"), S(7));
  PostArc(T("c"), S(8));
  PostArc(T("d"), S(9));
  ]
;;

let i = InhibitorSet.of_list [InhibArc(S(3), T("c"))];;

let m = PlaceSet.of_list [S(1); S(2); S(3); S(4); S(5)];;

let c1 = (s, t, f, i, m);;
print_ipt c1;;

let a = TransitionSet.of_list [T("a"); T("b"); T("c")];;
let preset_a = preset_of_TransitionSet a f;;
let postset_a = postset_of_TransitionSet a f;;

print_endline "prova preset";;
print_PlaceSet preset_a;;

print_endline "prova postset";;
print_PlaceSet postset_a;;

print_endline "prova enabled";;
print_bool (is_enabled a c1);;
