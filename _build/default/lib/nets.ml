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


    (* returns true iff t <. t'*)
    let caused_by t' t cn = 
      let preset_t = preset_of_transition t cn in
      let inhibitors_t' = inhibitors_of_transition t' cn in 
      not (PlaceSet.is_empty (PlaceSet.inter preset_t inhibitors_t'))


    let conflict_with t t' cn = 
      let preset_t = preset_of_transition t cn in
      let preset_t' = preset_of_transition t' cn in
      not (PlaceSet.is_empty (PlaceSet.inter preset_t preset_t'))

end;;


module CN = 
struct
  
include IPT 

  module Causality = 
  struct
    type t = {cause : transition ; effect : transition}
    let compare = compare

    let cause_of {cause = x; effect = _} = x
    let effect_of {cause = _; effect = x} = x

    let to_string {cause = t; effect = t'} = (Transition.to_string t) ^ " < " ^ (Transition.to_string t')
    let print x = print_endline (to_string x)

  end;;

  module CausalityRelation = 
  struct

    include Set.Make(Causality)

    let print s = iter Causality.print s

    let build cn = 
      let helper t tt = 
        TransitionSet.fold 
        (fun x cc -> add {cause = x; effect = t} cc)
        tt
        empty
      in

      let helper_causes_of t cn = 
        TransitionSet.fold 
        (fun x tt -> if caused_by t x cn then TransitionSet.add x tt else tt)
        (TransitionSet.remove t cn.transitions)
        TransitionSet.empty 
      in
      
      TransitionSet.fold 
      (fun x cc -> union (helper x (helper_causes_of x cn)) cc)
      cn.transitions
      empty
    
    let causes_of t cr = fold
      (fun c tt -> if (Causality.effect_of c) = t then TransitionSet.add (Causality.cause_of c) tt else tt)
      cr
      TransitionSet.empty


    let effects_of t cr = fold
    (fun c tt -> if (Causality.cause_of c) = t then TransitionSet.add (Causality.effect_of c) tt else tt)
    cr
    TransitionSet.empty

    let causes cr = fold 
      (fun x tt-> TransitionSet.add (Causality.cause_of x) tt)
      cr 
      TransitionSet.empty

    let effects cr = fold 
      (fun x tt-> TransitionSet.add (Causality.effect_of x) tt)
      cr 
      TransitionSet.empty

    let is_irreflexive cr = TransitionSet.fold
      (fun x b -> not (mem {cause = x ; effect = x} cr) && b)
      (causes cr)
      true
    
    let is_asymmetric cr = fold
      (fun {cause = c; effect = e} b -> not(mem {cause = e; effect = c} cr) && b)
      cr
      true


    (* if a <. b and b <.c then a <. c *)
    let is_transitive cr = 
      let helper bb cr = TransitionSet.fold 
        (fun b cc -> TransitionSet.union (causes_of b cr) cc)
        bb
        TransitionSet.empty
      in

      TransitionSet.fold
      (fun a b -> (TransitionSet.subset (helper (causes_of a cr) cr) (causes_of a cr)) && b)
      (causes cr)
      true

    let is_IPO cr = (is_irreflexive cr) && (is_asymmetric cr) && (is_transitive cr)
    
  end;;

  let causality_relation cn = CausalityRelation.build cn


  let non_flow_causality cn = 
    TransitionSet.for_all 
    (fun t -> 
      PlaceSet.for_all (fun s -> TransitionSet.is_empty (preset_of_place s cn)) (preset_of_transition t cn)
      )
    cn.transitions

    
  let no_backward_conflicts cn = 
      let postset_of_others t cn = 
        postset_of_TransitionSet (TransitionSet.remove t cn.transitions) cn in 
      
      TransitionSet.for_all 
      (fun t -> PlaceSet.is_empty (PlaceSet.inter (postset_of_transition t cn) (postset_of_others t cn)))
      cn.transitions
    
  let no_or_causality cn =
    let preset_of_others t cn = 
      preset_of_TransitionSet (TransitionSet.remove t cn.transitions) cn in 
    
    TransitionSet.for_all 
    (fun t -> PlaceSet.is_empty 
      (PlaceSet.inter 
        (inhibitors_of_TransitionSet cn.transitions cn)
        (PlaceSet.inter
          (preset_of_transition t cn)
          (preset_of_others t  cn)
        )
      )
    )
    cn.transitions
  
   (* There is a propriety that requires the inhibitors of a transition to be finite, here we can only represent finite sets, hence it is verified *)

   let ipo_causality cn = CausalityRelation.is_IPO (CausalityRelation.build cn)

  (* 6th condition on hold, not different from building the causality relation *)

  let correct_marking cn = 
    let b1 = PlaceSet.equal (postset_of_TransitionSet cn.transitions cn) cn.marking in
    let b2 = PlaceSet.subset (inhibitors_of_TransitionSet cn.transitions cn) cn.marking in

    b1 && b2

end;;