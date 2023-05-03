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

      let build s t f i m net = 
        net.places <- s ;
        net.transitions <- t;
        net.flow <- f;
        net.inhibitors <- i;
        net.marking <- m

      (* We say that an arc n1 -> n2 is valid iff n1 is a Place that belongs to S and n2 is a Transition that belongs to T or viceversa *)
      let valid_arc (arc : Flow.t) ipt = match arc with
        {source = S(n); target = T(s)}
      | {source = T(s); target = S(n)} -> (PlaceSet.mem n ipt.places) && (TransitionSet.mem s ipt.transitions)
      | _ -> raise IllegalArc

      (* We say that the flow relation is valid iff all the arcs of the relation itself are valid*)
      let valid_flow ipt = 
        FlowSet.for_all
        (fun arc -> valid_arc arc ipt)
        ipt.flow
        

      (* Function that pretty prints an IPT with all its sets*)
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

    (* This function returns the transitions for which the place s is the output *)
    let preset_of_place s ipt =

      let f' = FlowSet.filter (fun y -> y.target = S(s)) ipt.flow in 
      let preset = FlowSet.fold (fun x tt -> TransitionSet.add (match x.source with S(_) -> raise IllegalArc | T(t) -> t) tt) 
      f' 
      TransitionSet.empty in 
      preset

    (* This function returns the transitions for which the place s is the input *)
    let postset_of_place s ipt =

      let f' = FlowSet.filter (fun y -> y.source = S(s)) ipt.flow in 
      let postset = FlowSet.fold (fun x tt -> TransitionSet.add (match x.target with S(_) -> raise IllegalArc | T(t) -> t) tt) 
      f' 
      TransitionSet.empty in 
      
      postset

    (* This function returns the places which are the inputs of the transition t *)
    let preset_of_transition t ipt =

      let f' = FlowSet.filter (fun y -> y.target = T(t)) ipt.flow in 
      let preset = FlowSet.fold (fun x ss -> PlaceSet.add (match x.source with T(_) -> raise IllegalArc | S(s) -> s) ss) 
      f' 
      PlaceSet.empty in 
      
      preset

    (* This function returns the places which are the outputs of the transition t *)
    let postset_of_transition t ipt =

      let f' = FlowSet.filter (fun y -> y.source = T(t)) ipt.flow in 
      let postset = FlowSet.fold (fun x ss -> PlaceSet.add (match x.target with T(_) -> raise IllegalArc | S(s) -> s) ss) 
      f' 
      PlaceSet.empty in 
      
      postset

    (* This function returns the places which are the inhibitors of the transition t *)
    let inhibitors_of_transition t ipt =

      let f' = InhibitorSet.filter (fun y -> y.target = T(t)) ipt.inhibitors in 
      let inhibitors = InhibitorSet.fold (fun x ss -> PlaceSet.add (match x.source with T(_) -> raise IllegalArc | S(n) -> n) ss) 
      f' 
      PlaceSet.empty in 
      
    inhibitors
  
    (* Given a set of transitions a, this function returns the set of places that is the union of all presets of the transitions in a *)
    let preset_of_TransitionSet a ipt = 
      TransitionSet.fold (fun t ss -> PlaceSet.union (preset_of_transition t ipt) ss) a PlaceSet.empty

    (* Given a set of transitions a, this function returns the set of places that is the union of all postsets of the transitions in a *)
    let postset_of_TransitionSet a ipt = 
      TransitionSet.fold (fun t ss -> PlaceSet.union (postset_of_transition t ipt) ss) a PlaceSet.empty
    
    (* Given a set of transitions a, this function returns the set of places that is the union of all inhibitors of the transitions in a *)
    let inhibitors_of_TransitionSet a ipt =
      TransitionSet.fold (fun t ss -> PlaceSet.union (inhibitors_of_transition t ipt) ss) a PlaceSet.empty

    (* Given a set of transitions a, this function returns true iff a is enabled at the current marking of the ipt *)
    let is_enabled a ipt =
      let preset_a = preset_of_TransitionSet a ipt in
      let postset_a = postset_of_TransitionSet a ipt in
      let inhibset_a = inhibitors_of_TransitionSet a ipt in 
    
      (PlaceSet.subset preset_a ipt.marking)
      &&
      (PlaceSet.for_all (fun x ->  not (PlaceSet.mem x ipt.marking) && not (PlaceSet.mem x postset_a)) inhibset_a)
    
    
    (* Given a set of transitions a, this function fires a at the current marking of ipt,
        only if a is enabled, and if so, it then modifies the marking of the ipt*)
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

    (* returns true iff t #. t'*)
    let in_conflict t t' cn = 
      let preset_t = preset_of_transition t cn in
      let preset_t' = preset_of_transition t' cn in
      not (PlaceSet.is_empty (PlaceSet.inter preset_t preset_t'))

    let non_shared_inputs t ipt = 

      let t_set = TransitionSet.of_list [t] in

      PlaceSet.fold 
      (fun x ss -> 
        if TransitionSet.equal (postset_of_place x ipt) t_set then 
          PlaceSet.add x ss 
        else ss)
      (preset_of_transition t ipt)
      PlaceSet.empty

end;;

module Causality = 
struct
  type t = {cause : transition ; effect : transition}
  let compare = compare

  (* Given the tuple t <. t', returns t *)
  let cause_of {cause = x; effect = _} = x

  (* Given the tuple t <. t', returns t' *)
  let effect_of {cause = _; effect = x} = x

  let to_string {cause = t; effect = t'} = (Transition.to_string t) ^ " < " ^ (Transition.to_string t')
  let print x = print_endline (to_string x)

end;;

module CausalityRelation = 
struct

  include Set.Make(Causality)

  let print s = iter Causality.print s

  (* Given an ipt, this function returns the set representing the Causality Relation of that ipt *)
  let build ipt =

    (* This auxilary function builds a set of Causality tuples in which t is always the effect *)
    let helper t tt = 
      TransitionSet.fold 
      (fun x cc -> add {cause = x; effect = t} cc)
      tt
      empty
    in

    (* This auxilary function returns a set of transitions that represents the causes of the transition t *)
    let helper_causes_of t ipt = 
      TransitionSet.fold 
      (fun x tt -> if IPT.caused_by t x ipt then TransitionSet.add x tt else tt)
      (TransitionSet.remove t ipt.transitions)
      TransitionSet.empty 
    in
    
    TransitionSet.fold 
    (fun x cc -> union (helper x (helper_causes_of x ipt)) cc)
    ipt.transitions
    empty
  
  (* This function returns the set of transitions that are all the causes of the transition t

     side note: more efficient than the helper function defined above,
      because this uses the tuples of the Causality Relation alredy built *)
  let causes_of t cr = fold
    (fun c tt -> if (Causality.effect_of c) = t then TransitionSet.add (Causality.cause_of c) tt else tt)
    cr
    TransitionSet.empty

  (* This function returns the set of transitions that are all the effects of the transition t *)
  let effects_of t cr = fold
  (fun c tt -> if (Causality.cause_of c) = t then TransitionSet.add (Causality.effect_of c) tt else tt)
  cr
  TransitionSet.empty
  
  (* This function returns the set of transitions that are the cause of some transition t *)
  let causes cr = fold 
    (fun x tt-> TransitionSet.add (Causality.cause_of x) tt)
    cr 
    TransitionSet.empty
  (* This function returns the set of transitions that are the effect of some transition t *)
  let effects cr = fold 
    (fun x tt-> TransitionSet.add (Causality.effect_of x) tt)
    cr 
    TransitionSet.empty
  
  (* This function checks if the causality relation is irreflecive, i.e. not(a <. a) *)
  let is_irreflexive cr = TransitionSet.fold
    (fun x b -> not (mem {cause = x ; effect = x} cr) && b)
    (causes cr)
    true
  
  (* This function checks if the causality relation is asymmetric, i.e. if a <. b and b <. a then a = b *)
  let is_asymmetric cr = fold
    (fun {cause = c; effect = e} b -> not(mem {cause = e; effect = c} cr) && b)
    cr
    true


  (* This function checks if the causality relation is transitive, i.e. if a <. b and b <.c then a <. c *)
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

  (* This function check if the causality relation is an Irreflexive Partial Order,
      i.e. if it is irreflexive, asymmetric and transitive *)
  let is_IPO cr = (is_irreflexive cr) && (is_asymmetric cr) && (is_transitive cr)
  
end;;


module Conflict = 
struct
  type t = {t1 : transition ; t2 : transition}  
  let compare = compare 

  let to_string tt = Transition.to_string tt.t1 ^ " # " ^ Transition.to_string tt.t2

  let print x = print_endline (to_string x)

end;;

module ConflictRelation = 
struct
  include Set.Make(Conflict) 
  let print s = iter Conflict.print s

   (* Given an ipt, this function returns the set representing the Conflict Relation of that ipt  *)
  let build ipt = 

    (* Given a transition t, this helper function returns the Conflict Relation made of the couples (t1,t2),
        where t1 = t and t2 is some transition in conflict with t *)
    let helper t ipt = 
      TransitionSet.fold
      (fun x cc -> if IPT.in_conflict t x ipt then add {t1 = t ; t2 = x} cc else cc)
      (TransitionSet.remove t ipt.transitions)
      empty

    in

    TransitionSet.fold
    (fun x cc -> union (helper x ipt) cc)
    ipt.transitions
    empty

end;;


module CN = 
struct
  
include IPT 

  (* Given an ipt, this function return its causal relation *)
  let causality_relation ipt = CausalityRelation.build ipt

  (* Given an ipt, this function return its conflict relation *)
  let conflict_relation ipt = ConflictRelation.build ipt

  let conflict_free_set x ipt = 
    ConflictRelation.for_all 
    (fun tt -> not ((TransitionSet.mem tt.t1 x) && (TransitionSet.mem tt.t2 x)))
    (conflict_relation ipt)
  
  let leftclosed_causality_set x net = 
    TransitionSet.for_all
    (fun t -> TransitionSet.subset (CausalityRelation.causes_of t (causality_relation net)) x)
    x

  (* This function returns true iff the flow relation of the ipt does not define causality,
      i.e. if all the places are not inputs and outputs of transitions *)
  let non_flow_causality ipt = 
    TransitionSet.for_all 
    (fun t -> 
      PlaceSet.for_all (fun s -> TransitionSet.is_empty (preset_of_place s ipt)) (preset_of_transition t ipt)
      )
      ipt.transitions

  (* This function returns true iff there are no backwards conflicts in the ipt,
      i.e. a place belongs to at most to the postset of one transition *)  
  let no_backward_conflicts ipt = 
      let postset_of_others t ipt = 
        postset_of_TransitionSet (TransitionSet.remove t ipt.transitions) ipt in 
      
      TransitionSet.for_all 
      (fun t -> PlaceSet.is_empty (PlaceSet.inter (postset_of_transition t ipt) (postset_of_others t ipt)))
      ipt.transitions
    
  (* This function returns true iff the ipt has no or-causality,
      i.e. situations in which the the firing of a transition may have different causes *)
  let no_or_causality ipt =
    let preset_of_others t ipt = 
      preset_of_TransitionSet (TransitionSet.remove t ipt.transitions) ipt in 
    
    TransitionSet.for_all 
    (fun t -> PlaceSet.is_empty 
      (PlaceSet.inter 
        (inhibitors_of_TransitionSet ipt.transitions ipt)
        (PlaceSet.inter
          (preset_of_transition t ipt)
          (preset_of_others t  ipt)
        )
      )
    )
    ipt.transitions
  
   (* There is a propriety that requires the inhibitors of a transition to be finite, here we can only represent finite sets,
       hence it is verified *)

   (* This function checks that the causality relation of the ipt is an irreflexive partial order,
       as a consequence, if the ipt has no cycles in the dependencies arising from inhibitors *)
   let ipo_causality ipt = CausalityRelation.is_IPO (causality_relation ipt)

  (* This functions checks that the set made of the causes of some transition t is conflict-free *)
  let local_conflict_freeness ipt = 
    TransitionSet.for_all
    (fun t -> conflict_free_set (CausalityRelation.causes_of t (causality_relation ipt)) ipt)
    ipt.transitions 

  (* This function checks that the initial marking of the ipt is a subset of the preset of all the transitions,
      and that inhibitors of the ipt have places only present in the initial marking *)
  let correct_marking ipt = 
    let b1 = PlaceSet.equal (preset_of_TransitionSet ipt.transitions ipt) ipt.marking in
    let b2 = PlaceSet.subset (inhibitors_of_TransitionSet ipt.transitions ipt) ipt.marking in

    b1 && b2
  
  (* This function checks if an ipt is also a pre-Causal Net, 
     i.e. if all the proprieties described are verified in such ipt *)
  let is_pCN ipt = 
    (non_flow_causality ipt) &&
    (no_backward_conflicts ipt) &&
    (no_or_causality ipt) &&
    (ipo_causality ipt) && 
    (local_conflict_freeness ipt) &&
    (correct_marking ipt)

  (* This function checks if the Conflict Relation is inherited along the Causality Relation, 
     i.e. if t #. t' and t' <. t'' then t #. t'' *)
  let is_conflict_inherited ipt = 
    ConflictRelation.for_all 
    (fun x -> 
      TransitionSet.for_all 
      (fun y -> ConflictRelation.mem {t1 = x.t1 ; t2 = y} (conflict_relation ipt))
      (CausalityRelation.effects_of x.t2 (causality_relation ipt))
      )
    (conflict_relation ipt)

  (* This function checks if the given ipt is a CN, 
     i.e. if the ipt is a pre-Causal Net and the Conflict Relation is inherited along the Causality Relation *)
  let is_CN ipt = 
    (is_pCN ipt) && (is_conflict_inherited ipt)

  (* This function returns true iff the set of transitions x is a configuration for the net *)
  let is_configuration x net = 
    if is_pCN net then 
      (TransitionSet.subset x net.transitions) &&
      (conflict_free_set x net) && 
      (leftclosed_causality_set x net)
    else 
      raise IllegalNet
end;;

module ReversibleCN = 
struct

include CN

  let forward_transitions ipt = TransitionSet.forward_subset ipt.transitions 
  let backward_transitions ipt = TransitionSet.backward_subset ipt.transitions

  let forward_subnet_is_pCN ipt = 
    let forward_flow = 
      FlowSet.filter
      (function {source = n1 ; target = n2} -> match (n1, n2) with 
        | S(_) , T(F _ )
        | T(F _ ), S(_) -> true 
        | _ -> false
      )
      ipt.flow
    in 

    let forward_inhibitors = 
      InhibitorSet.filter
      (function {source = n1 ; target = n2} -> match (n1, n2) with 
        | S(_) , T(F _ )
        | _ -> false
      )
      ipt.inhibitors
    in 

    let subnet : IPT.t = 
      {places = ipt.places; 
       transitions = forward_transitions ipt;
       flow = forward_flow;
       inhibitors = forward_inhibitors;
       marking = ipt.marking
      }
    in

    is_pCN subnet
  
  let exaclty_one_reverse_transition ipt = 
    let at_most_one_revesing_transition = 
      let preset_of_others t ipt = 
        preset_of_TransitionSet (TransitionSet.remove t ipt.transitions) ipt 
      in 
      
      TransitionSet.for_all 
      (fun t -> PlaceSet.is_empty (PlaceSet.inter (preset_of_transition t ipt) (preset_of_others t ipt)))
      (backward_transitions ipt)
    in


    at_most_one_revesing_transition

    (* Needs to finish this propriety*)


end;;