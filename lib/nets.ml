open Sets
open Exceptions

module IPT = 
struct

      type t = {
        mutable places : PlaceSet.t;
        mutable transitions : TransitionSet.t;
        mutable flow : FlowSet.t;
        mutable inhibitors : InhibitorSet.t;
        mutable marking : PlaceSet.t;
      }

      let valid_arc (arc : Flow.t) ipt = match arc with
        {source = S(n); target = T(s)}
      | {source = T(s); target = S(n)} -> (PlaceSet.mem n ipt.places) && (TransitionSet.mem s ipt.transitions)
      | _ -> raise IllegalArc

      let valid_flow ipt = 
        FlowSet.for_all
        (fun arc -> valid_arc arc ipt)
        ipt.flow
        

      let print ipt = 
      print_endline "\nPlaces:";
      PlaceSet.print ipt.places;
    
      print_endline "\nTransitions:";
      TransitionSet.print ipt.transitions;
    
      print_endline "\nFlow Relation:";
      FlowSet.print ipt.flow;
    
      print_endline "\nInhibitor arcs:";
      InhibitorSet.print ipt.inhibitors;
    
      print_endline "\nMarking:";
      PlaceSet.print ipt.marking

    
    let preset_of_place s ipt =

      let f' = FlowSet.filter (fun y -> y.target = S(s)) ipt.flow in 
      let preset = FlowSet.fold (fun x tt -> TransitionSet.add (match x.source with S(_) -> raise IllegalArc | T(t) -> t) tt) 
      f' 
      TransitionSet.empty in 
      preset

    let postset_of_place s ipt =

      let f' = FlowSet.filter (fun y -> y.source = S(s)) ipt.flow in 
      let postset = FlowSet.fold (fun x tt -> TransitionSet.add (match x.target with S(_) -> raise IllegalArc | T(t) -> t) tt) 
      f' 
      TransitionSet.empty in 
      
      postset

    let preset_of_transition t ipt =

      let f' = FlowSet.filter (fun y -> y.target = T(t)) ipt.flow in 
      let preset = FlowSet.fold (fun x ss -> PlaceSet.add (match x.source with T(_) -> raise IllegalArc | S(s) -> s) ss) 
      f' 
      PlaceSet.empty in 
      
      preset

    let postset_of_transition t ipt =

      let f' = FlowSet.filter (fun y -> y.source = T(t)) ipt.flow in 
      let postset = FlowSet.fold (fun x ss -> PlaceSet.add (match x.target with T(_) -> raise IllegalArc | S(s) -> s) ss) 
      f' 
      PlaceSet.empty in 
      
      postset

    let inhibitors_of_transition t ipt =

      let f' = InhibitorSet.filter (fun y -> y.target = T(t)) ipt.inhibitors in 
      let inhibitors = InhibitorSet.fold (fun x ss -> PlaceSet.add (match x.source with T(_) -> raise IllegalArc | S(n) -> n) ss) 
      f' 
      PlaceSet.empty in 
      
    inhibitors
  

    let preset_of_TransitionSet s ipt = 
      TransitionSet.fold (fun t ss -> PlaceSet.union (preset_of_transition t ipt) ss) s PlaceSet.empty

    let postset_of_TransitionSet s ipt = 
      TransitionSet.fold (fun t ss -> PlaceSet.union (postset_of_transition t ipt) ss) s PlaceSet.empty
    
    
    let inhibitors_of_TransitionSet s ipt =
      TransitionSet.fold (fun t ss -> PlaceSet.union (inhibitors_of_transition t ipt) ss) s PlaceSet.empty

    let is_enabled a ipt =
      let preset_a = preset_of_TransitionSet a ipt in
      let postset_a = postset_of_TransitionSet a ipt in
      let inhibset_a = inhibitors_of_TransitionSet a ipt in 
    
      (PlaceSet.subset preset_a ipt.marking)
      &&
      (PlaceSet.for_all (fun x ->  not (PlaceSet.mem x ipt.marking) && not (PlaceSet.mem x postset_a)) inhibset_a)
    
    
    (* Conflicts and other relations are yet to be included *)
    let fire_seq a ipt =
      let preset_a = preset_of_TransitionSet a ipt in
      let postset_a = postset_of_TransitionSet a ipt in

      if is_enabled a ipt then
        ipt.marking <- PlaceSet.union (PlaceSet.diff ipt.marking preset_a) postset_a
end;;