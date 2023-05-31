open Tirocinio.Sets;;
open Tirocinio.Pes;;
open Tirocinio.Relations;;

let e = TransitionSet.of_list [F "a"; F "b"; F "c"; F "d"];;

let u = TransitionSet.of_list [F "b"; F "c"];;

let cf = ConflictRelation.of_list [
  {t1 = F "a" ; t2 = F "b"};
  {t1 = F "b" ; t2 = F "a"};
];;

let cr = CausalityRelation.of_list [
  {cause = F "b" ; effect = F "c"};
];;

let rev_cr = ReverseCausalityRelation.of_list [
  {cause = F "b"; rev = B "b"};
  {cause = F "c"; rev = B "c"};
  {cause = F "d"; rev = B "c"};
];;

let prev = PreventionRelation.of_list [];;

let p4 : ReversiblePES.t = 
  {events = e ;
   undoable_events = u;
   conflict = cf;
   causality = cr;
   rev_causality = rev_cr;
   prevention = prev;
   current_configuration = TransitionSet.empty;
   };;