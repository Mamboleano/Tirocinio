open Tirocinio.Sets;;
open Tirocinio.Pes;;
open Tirocinio.Relations;;

let e = TransitionSet.of_list [F "a"; F "b"; F "c"; F "d"];;

let cf = ConflictRelation.of_list [
  {t1 = F "a" ; t2 = F "b"};
  {t1 = F "b" ; t2 = F "a"};
  {t1 = F "c" ; t2 = F "a"};
  {t1 = F "a" ; t2 = F "c"};
];;

let cr = CausalityRelation.of_list [
  {cause = F "b" ; effect = F "c"};
];;

let p2 : PES.t = {events = e ; conflict = cf; causality = cr ; current_configuration = TransitionSet.empty};;
