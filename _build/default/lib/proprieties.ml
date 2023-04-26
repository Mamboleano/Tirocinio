open Utils
open Types

let is_enabled a (_, _, f, i, m) = 
  let preset_a = preset_of_TransitionSet a f in
  let postset_a = postset_of_TransitionSet a f in
  let inhibset_a = inhibitors_of_TransitionSet a i in 

  (PlaceSet.subset preset_a m)
  &&
  (PlaceSet.for_all (fun x ->  not (PlaceSet.mem x m) && not (PlaceSet.mem x postset_a)) inhibset_a)
;;
  