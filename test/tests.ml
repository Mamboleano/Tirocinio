
open Tirocinio.Nets;;
open Tirocinio.Pes;;
open Tirocinio.Sets;;
open Tirocinio.Relations;;
open Examples.Net1;;
open Examples.Net2;;
open Examples.Net3;;
open Examples.Net4;;
open Examples.Net5;;
open Examples.Pes1;;
open Examples.Pes2;;
open Examples.Pes3;;
open Examples.Pes4;;
open Examples.Pes5;;
open Tirocinio.Mappings;;

assert(CN.is_pCN c1);;
assert(CN.is_CN c2);;
assert(ReversibleCN.is_rCN c3);;
assert(ReversibleCN.is_rCN c4);;

assert(PES.is_pPES p1);;
assert(not (PES.is_PES p1));;

assert(PES.is_pPES p2);;
assert(PES.is_PES p2);;

assert(ReversiblePES.is_rPES p4);;


assert(
  List.for_all 
    (fun c -> PES.is_enabled_at c (TransitionSet.empty) p1)
    confs_enabled
    );;

assert(
  List.for_all 
    (fun c -> not (PES.is_enabled_at c (TransitionSet.empty) p1))
    confs_disabled
    );;

assert(ReversiblePES.is_rPES p3);;
assert(CausalityRelation.equal (ReversiblePES.sustained_causation p3) (p3.causality));;


assert(not (ReversiblePES.is_enabled_at (TransitionSet.singleton (F("d"))) (TransitionSet.singleton (B("b"))) (TransitionSet.of_list [F("b"); F("c")]) p3));;
assert((ReversiblePES.is_enabled_at (TransitionSet.singleton (F("d"))) (TransitionSet.singleton (B("c"))) (TransitionSet.of_list [F("b"); F("c")]) p3));;

ReversiblePES.exec_seq seq_1 p3;;
assert(TransitionSet.equal (p3.current_configuration) (TransitionSet.of_list [F "a"; F "d"]));;
ReversiblePES.reset_conf p3;;

ReversiblePES.exec_seq 
  [
    (TransitionSet.singleton (F "b") , TransitionSet.empty);
    (TransitionSet.singleton (F "d") , TransitionSet.singleton (B "b"));
    (TransitionSet.singleton (F "a") , TransitionSet.empty);
  ]
  p3;;
assert(TransitionSet.equal (p3.current_configuration) (TransitionSet.of_list [F "a"; F "d"]));;
ReversiblePES.reset_conf p3;;


ReversiblePES.exec_seq seq_2 p3;;
assert(TransitionSet.equal (p3.current_configuration) (TransitionSet.singleton (F "d")));;
ReversiblePES.reset_conf p3;;

ReversiblePES.exec_seq seq_3 p3;;
assert(TransitionSet.equal (p3.current_configuration) (TransitionSet.of_list [F "b"; F "c"; F "d"]));;
ReversiblePES.reset_conf p3;;

let conv_p1 = to_pCN p1;;
assert(CN.is_pCN conv_p1);;
assert(not(CN.is_CN conv_p1));;

let conv_p2 = to_pCN p2;;
(*CN.print conv_p2;;*)
assert(CN.is_pCN conv_p2);;
assert(CN.is_CN conv_p2);;

let conv_c1 = to_pPES c1;;
(* PES.print conv_c1;; *)
assert(PES.is_pPES conv_c1);;
assert(not(PES.is_PES conv_c1));;


let conv_c2 = to_pPES c2;;
(*PES.print conv_c2;;*)
assert(PES.is_PES conv_c2);;

let conv_c3 = to_rPES c3;;
(* ReversiblePES.print conv_c3;; *)
assert(ReversiblePES.is_rPES conv_c3);;

let conv_p3 = to_rCN p3;;
(* ReversibleCN.print conv_p3;; *)
assert(ReversibleCN.is_rCN conv_p3);;

let conv_p4 = to_rCN p4;;
assert(ReversibleCN.is_rCN conv_p4);;

assert(ReversiblePES.is_cause_respecting p3);;
(*let exec_seq_1 = *)ReversiblePES.is_reachable_conf_CR (TransitionSet.of_list [F("b"); F("c"); F("d");]) p3;;
(* List.iter (fun x -> Printf.printf "{";
                  TransitionSet.print x;
                  Printf.printf "}") exec_seq_1; *)


(* Testing causally bothering set *)
assert(ReversibleCN.is_cause_respecting c3);;

assert(not(ReversibleCN.is_cause_respecting c4));;
assert(ReversibleCN.is_cause_respecting (ReversibleCN.to_causally_respecting_net c4));;

assert(ReversibleCN.is_rCN c5);;
assert(not(ReversibleCN.is_cause_respecting c5));;


assert(not (ReversibleCN.is_enabled_at (TransitionSet.singleton (F("d"))) (TransitionSet.singleton (B("c"))) (PlaceSet.of_list [I(1); I(5); I(7); I(8)]) c3));;
assert((ReversibleCN.is_enabled_at (TransitionSet.empty) (TransitionSet.singleton (B("c"))) (PlaceSet.of_list [I(1); I(7); I(8); I(9)]) c3));;

ReversibleCN.fire_seq 
  [
    (TransitionSet.of_list [F("b") ; F("d")] , TransitionSet.empty); 
    (TransitionSet.of_list [F("c")] , TransitionSet.empty);
    (TransitionSet.empty , TransitionSet.of_list [B("c")]);
  ]
  c3;;

assert(PlaceSet.equal c3.current_marking (PlaceSet.of_list [I(0); I(1) ; I(4); I(7); I(9)]));;
ReversibleCN.reset_marking c3;;

(*
let firing_seq_1 = ReversibleCN.is_reachable_conf_CR (TransitionSet.of_list [F("b"); F("c"); F("d")]) c3;;
List.iter (fun x -> Printf.printf "{";
                  TransitionSet.print x;
                  Printf.printf "}") firing_seq_1;

*)

(* ReversibleCN.exec_seq 
  [
    (TransitionSet.singleton (F "b") , TransitionSet.empty);
    (TransitionSet.singleton (F "d") , TransitionSet.singleton (B "b"));
    (TransitionSet.singleton (F "c") , TransitionSet.empty);
    (TransitionSet.empty, TransitionSet.singleton (F "c"));
  ]
  c3;;
assert(TransitionSet.equal (p3.current_configuration) (TransitionSet.of_list [F "a"; F "d"]));;
ReversiblePES.reset_conf p3;;
*)



(* TransitionSet.print (ReversibleCN.causal_bothering_set c5);; *)
(*ReversiblePES.print (to_rPES c5);;*)

(*
CausalityRelation.print (ReversiblePES.sustained_causation p3);;
let prova_list = ReversiblePES.order_transition_sets_with_causality (p3.events) (ReversiblePES.sustained_causation p3);;
List.iter (TransitionSet.print) prova_list;;
*)

assert(ReverseCausalityRelation.is_causal_reversibility p5.rev_causality);;

(* let exec_seq_2 = *)ReversiblePES.is_reachable_conf (TransitionSet.of_list ([F"c"; F"a"])) p5;;
(* print_endline (TransitionSet.list_to_string exec_seq_2);; *)

ReversiblePES.is_reachable_conf (TransitionSet.of_list ([F"d"; F"c"])) p5;;
ReversiblePES.is_reachable_conf (TransitionSet.of_list ([F"c"; F"a"; F"d"])) p5;;
ReversiblePES.is_reachable_conf (TransitionSet.of_list ([F"c"; F"d"])) p5;;
ReversiblePES.is_reachable_conf (TransitionSet.of_list ([F"a"])) p5;;
ReversiblePES.is_reachable_conf (TransitionSet.of_list ([F"b"])) p5;;
ReversiblePES.is_reachable_conf (TransitionSet.of_list ([F"c"])) p5;;
ReversiblePES.is_reachable_conf (TransitionSet.of_list ([F"d"])) p5;;
ReversiblePES.is_reachable_conf (TransitionSet.of_list ([F"d"; F"a"])) p5;;
ReversiblePES.is_reachable_conf (TransitionSet.of_list ([F"a"; F"a"])) p5;;
ReversiblePES.is_reachable_conf (TransitionSet.of_list ([F"d"; F"a"; F"c"])) p5;;

assert(not (ReversibleCN.is_cause_respecting c4));;
assert(ReverseCausalityRelation.is_causal_reversibility (ReversibleCN.reverse_causality_relation c4));;

ReversibleCN.is_reachable_conf (TransitionSet.of_list ([F"d"; F"c"])) c4;;
let fire_seq_2 = ReversibleCN.is_reachable_conf (TransitionSet.of_list ([F"a"; F"c"])) c4;;
print_endline (TransitionSet.list_to_string fire_seq_2);;
ReversibleCN.is_reachable_conf (TransitionSet.of_list ([F"c"; F"a"; F"d"])) c4;;
ReversibleCN.is_reachable_conf (TransitionSet.of_list ([F"c"; F"d"])) c4;;
ReversibleCN.is_reachable_conf (TransitionSet.of_list ([F"a"])) c4;;
ReversibleCN.is_reachable_conf (TransitionSet.of_list ([F"b"])) c4;;
ReversibleCN.is_reachable_conf (TransitionSet.of_list ([F"c"])) c4;;
ReversibleCN.is_reachable_conf (TransitionSet.of_list ([F"d"])) c4;;
ReversibleCN.is_reachable_conf (TransitionSet.of_list ([F"d"; F"a"])) c4;;
ReversibleCN.is_reachable_conf (TransitionSet.of_list ([F"a"; F"a"])) c4;;
ReversibleCN.is_reachable_conf (TransitionSet.of_list ([F"d"; F"a"; F"c"])) c4;;


print_endline "passed all tests";;
