open Tirocinio.Sets;;
open Tirocinio.Pes;;
open Tirocinio.Relations;;

let e = TransitionSet.of_list [F "a"; F "b"; F "c"; F "d"];;

let u = TransitionSet.of_list [F "b"; F "c"];;

let cf = ConflictRelation.of_list [
  {t1 = F "a" ; t2 = F "b"};
  {t1 = F "b" ; t2 = F "a"};
  {t1 = F "c" ; t2 = F "a"};
  {t1 = F "a" ; t2 = F "c"};
];;

let cr = CausalityRelation.of_list [
  {cause = F "b" ; effect = F "c"};
];;

let rev_cr = ReverseCausalityRelation.of_list [
  {cause = F "b"; rev = B "b"};
  {cause = F "c"; rev = B "c"};
];;

let prev = PreventionRelation.of_list [
  {preventing = F "c"; prevented = B "b"};
];;

let p3 : ReversiblePES.t = 
  {events = e ;
   undoable_events = u;
   conflict = cf;
   causality = cr;
   rev_causality = rev_cr;
   prevention = prev;
   current_configuration = TransitionSet.empty;
   };;

let seq_1 = [
  (TransitionSet.singleton (F "a") , TransitionSet.empty);
  (TransitionSet.singleton (F "d") , TransitionSet.empty);
];;

let seq_2 = [
  (TransitionSet.of_list [F "b"; F "d"] , TransitionSet.empty);
  (TransitionSet.empty , TransitionSet.singleton (B "b"));
];;

let seq_3 = [
  (TransitionSet.singleton (F "b") , TransitionSet.empty);
  (TransitionSet.singleton (F "c") , TransitionSet.empty);
  (TransitionSet.singleton (F "d") , TransitionSet.singleton (B "c"));
  (TransitionSet.singleton (F "c") , TransitionSet.empty);
];;