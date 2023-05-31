open Tirocinio.Sets;;
open Tirocinio.Pes;;
open Tirocinio.Relations;;

let e = TransitionSet.of_list [F "a"; F "b"; F "c"; F "d"];;

let cf = ConflictRelation.of_list [
  {t1 = F "a" ; t2 = F "b"};
  {t1 = F "b" ; t2 = F "a"};
];;

let cr = CausalityRelation.of_list [
  {cause = F "b" ; effect = F "c"};
];;

let p1 : PrePES.t = {events = e ; conflict = cf; causality = cr ; current_configuration = TransitionSet.empty};;

let confs_enabled = [
  TransitionSet.singleton (F("a"));
  TransitionSet.singleton (F("b"));
  TransitionSet.singleton (F("d"));
  (TransitionSet.of_list [F "a" ; F "d"]);
  (TransitionSet.of_list [F "b" ; F "d"]);
];;

let confs_disabled = [
  TransitionSet.singleton (F("c"));
  (TransitionSet.of_list [F "a" ; F "b"]);
];;






