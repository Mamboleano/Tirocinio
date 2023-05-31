
open Tirocinio.Nets;;
open Tirocinio.Pes;;
open Tirocinio.Sets;;
open Tirocinio.Relations;;
open Examples.Net1;;
open Examples.Net2;;
open Examples.Net3;;
open Examples.Net4;;
open Examples.Pes1;;
open Examples.Pes2;;
open Examples.Pes3;;
open Examples.Pes4;;
open Tirocinio.Converters;;

assert(CN.is_pCN c1);;
assert(CN.is_CN c2);;
assert(ReversibleCN.is_rCN c3);;
assert(ReversibleCN.is_rCN c4);;

assert(PrePES.is_pPES p1);;
assert(not (PrePES.is_PES p1));;

assert(PrePES.is_pPES p2);;
assert(PrePES.is_PES p2);;

assert(ReversiblePES.is_rPES p4);;

assert(
  List.for_all 
    (fun c -> PrePES.is_enabled_at c (TransitionSet.empty) p1)
    confs_enabled
    );;

assert(
  List.for_all 
    (fun c -> not (PrePES.is_enabled_at c (TransitionSet.empty) p1))
    confs_disabled
    );;

assert(ReversiblePES.is_rPES p3);;
assert(CausalityRelation.equal (ReversiblePES.sustained_causation p3) (p3.causality));;

ReversiblePES.fire_seq seq_1 p3;;
assert(TransitionSet.equal (p3.current_configuration) (TransitionSet.of_list [F "a"; F "d"]));;
ReversiblePES.reset_conf p3;;

ReversiblePES.fire_seq seq_2 p3;;
assert(TransitionSet.equal (p3.current_configuration) (TransitionSet.singleton (F "d")));;
ReversiblePES.reset_conf p3;;

ReversiblePES.fire_seq seq_3 p3;;
assert(TransitionSet.equal (p3.current_configuration) (TransitionSet.of_list [F "b"; F "c"; F "d"]));;
ReversiblePES.reset_conf p3;;

let conv_p1 = to_pCN p1;;
assert(CN.is_pCN conv_p1);;
assert(not(CN.is_CN conv_p1));;

let conv_p2 = to_pCN p2;;
(* CN.print conv_p2;; *)
assert(CN.is_pCN conv_p2);;
assert(CN.is_CN conv_p2);;

let conv_c1 = to_pPES c1;;
(* PrePES.print conv_c1;; *)
assert(PrePES.is_pPES conv_c1);;
assert(not(PrePES.is_PES conv_c1));;


let conv_c2 = to_pPES c2;;
(* PrePES.print conv_c2;; *)
assert(PrePES.is_PES conv_c2);;

let conv_p3 = to_rCN p3;;
(* ReversibleCN.print conv_p3;; *)
assert(ReversibleCN.is_rCN conv_p3);;

let conv_p4 = to_rCN p4;;
assert(ReversibleCN.is_rCN conv_p4);;


(* Testing causally bothering set *)
assert(ReversibleCN.is_cause_respecting c3);;

assert(not(ReversibleCN.is_cause_respecting c4));;
assert(ReversibleCN.is_cause_respecting (ReversibleCN.to_causally_respecting_net c4));;

print_endline "passed all tests";;
