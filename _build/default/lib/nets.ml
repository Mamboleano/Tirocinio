open Sets;;
open Exceptions;;
open Relations;;

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
    let caused_by t' t ipt = 
      let preset_t = preset_of_transition t ipt in
      let inhibitors_t' = inhibitors_of_transition t' ipt in 
      not (PlaceSet.is_empty (PlaceSet.inter preset_t inhibitors_t'))
        
    (* returns true iff t #. t'*)
    let in_conflict t t' ipt = 
      let preset_t = preset_of_transition t ipt in
      let preset_t' = preset_of_transition t' ipt in
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


module CN = 
struct
  
include IPT 

  (* Given an ipt, this function return its causal relation *)
  let causality_relation ipt = 
    (* This auxilary function builds a set of Causality tuples in which t is always the effect *)
    let helper t tt = 
      TransitionSet.fold 
      (fun x cc -> CausalityRelation.add {cause = x; effect = t} cc)
      tt
      CausalityRelation.empty
    in

    (* This auxilary function returns a set of transitions that represents the causes of the transition t *)
    let helper_causes_of t ipt = 
      TransitionSet.fold 
      (fun x tt -> if IPT.caused_by t x ipt then TransitionSet.add x tt else tt)
      (TransitionSet.remove t ipt.transitions)
      TransitionSet.empty 
    in
    
    TransitionSet.fold 
    (fun x cc -> CausalityRelation.union (helper x (helper_causes_of x ipt)) cc)
    ipt.transitions
    CausalityRelation.empty

  (* Given an ipt, this function return its conflict relation *)
  let conflict_relation ipt = 
    (* Given a transition t, this helper function returns the Conflict Relation made of the couples (t1,t2),
      where t1 = t and t2 is some transition in conflict with t *)
      let helper t ipt = 
        TransitionSet.fold
        (fun x cc -> if IPT.in_conflict t x ipt then ConflictRelation.add {t1 = t ; t2 = x} cc else cc)
        (TransitionSet.remove t ipt.transitions)
        ConflictRelation.empty
  
      in
  
      TransitionSet.fold
      (fun x cc -> ConflictRelation.union (helper x ipt) cc)
      ipt.transitions
      ConflictRelation.empty

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

  let is_prevented_by u t' rCN = 
    if ((Transition.is_backward u) && ((Transition.is_forward t'))) then 
      not (PlaceSet.is_empty 
        (PlaceSet.inter 
          (postset_of_transition t' rCN ) 
          (inhibitors_of_transition u rCN)
        ))
    else
      raise IllegalPrevention

  let reverses_of t ipt = 
    TransitionSet.fold
      (fun x tt -> 
        if (PlaceSet.equal (preset_of_transition x ipt) (postset_of_transition t ipt)) then 
          TransitionSet.add x tt 
        else
          tt
        )
      (backward_transitions ipt)
      (TransitionSet.empty)

  
  let sustained_causation ipt = 
    CausalityRelation.fold
    (fun {cause = t ; effect = t'} sc -> 
      if ((Transition.is_forward t) && (Transition.is_forward t')) then
        if (not(TransitionSet.equal (reverses_of t ipt) (TransitionSet.empty))) then 
          if PlaceSet.is_empty (PlaceSet.inter (postset_of_transition t' ipt) (inhibitors_of_transition (B(Transition.label t)) ipt)) then 
            sc 
          else 
            CausalityRelation.add {cause = t ; effect = t'} sc
        else
          CausalityRelation.add {cause = t ; effect = t'} sc
      else 
        sc
    )
    (causality_relation ipt)
    (CausalityRelation.empty)


  let sustainly_caused_by t' t ipt = 
    CausalityRelation.mem {cause = t ; effect = t'} (sustained_causation ipt)


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
        preset_of_TransitionSet (TransitionSet.remove t (backward_transitions ipt)) ipt 
      in 
      
      TransitionSet.for_all 
      (fun t -> PlaceSet.is_empty (PlaceSet.inter (preset_of_transition t ipt) (preset_of_others t ipt)))
      (backward_transitions ipt)
    in
    
    let transitions_that_produce_input bt ipt = 
      TransitionSet.fold
      (fun ft tt -> 
        if PlaceSet.equal (postset_of_transition ft ipt) (preset_of_transition bt ipt) then 
          TransitionSet.add ft tt 
        else
          tt
      )
      (forward_transitions ipt)
      TransitionSet.empty
    in

    let transitions_that_take_output bt ipt = 
      TransitionSet.fold
      (fun ft tt -> 
        if PlaceSet.equal (preset_of_transition ft ipt) (postset_of_transition bt ipt) then 
          TransitionSet.add ft tt 
        else
          tt
      )
      (forward_transitions ipt)
      TransitionSet.empty
    in

    let transitions_that_cause_reversing bt ipt = 
      TransitionSet.fold
      (fun ft tt -> 
        if not (PlaceSet.equal (PlaceSet.inter (non_shared_inputs ft ipt) (inhibitors_of_transition bt ipt)) (PlaceSet.empty)) then 
          TransitionSet.add ft tt 
        else
          tt
      )
      (forward_transitions ipt)
      TransitionSet.empty
    in

    at_most_one_revesing_transition && 
    (
      TransitionSet.for_all 
      (fun t -> 
        TransitionSet.equal
          (TransitionSet.inter 
          (TransitionSet.inter (transitions_that_produce_input t ipt) (transitions_that_take_output t ipt))
          (transitions_that_cause_reversing t ipt)
          )

          (TransitionSet.of_list [F(Transition.label t)])  
      )
      (backward_transitions ipt)
    )


    (*
   let finite_causes_of_backward_transitions ipt = 

    This condition requires a set to be finite, hence it is satisfied since we cannot represent infitite sets
    *)

    let not_caused_and_prevented ipt = 
      TransitionSet.for_all
      (fun bt -> 
        TransitionSet.for_all 
        (fun ft -> 
          if not (PlaceSet.equal 
              (PlaceSet.inter (preset_of_transition ft ipt) (inhibitors_of_transition bt ipt))
              (PlaceSet.empty)
            ) then 

              (PlaceSet.equal 
            (PlaceSet.inter (postset_of_transition ft ipt) (inhibitors_of_transition bt ipt))
            (PlaceSet.empty)
              )
          else 
            true
          )
        (forward_transitions ipt))
      (backward_transitions ipt)
    

    let is_conflict_inherited_along_sustained_causality ipt = 
      TransitionSet.for_all 
      (fun t -> 
        TransitionSet.for_all 
        (fun t' -> 
          TransitionSet.for_all 
          (fun t'' -> 
            if (in_conflict t t' ipt) && (sustainly_caused_by t'' t' ipt) then 
              in_conflict t t'' ipt
            else 
              true
              )
          (TransitionSet.diff (forward_transitions ipt) (TransitionSet.of_list [t ; t']))
          )
        (TransitionSet.diff (forward_transitions ipt) (TransitionSet.singleton t))
      )
      (forward_transitions ipt)
    
    let is_rCN ipt = 
      (forward_subnet_is_pCN ipt) &&
      (exaclty_one_reverse_transition ipt) &&
      (not_caused_and_prevented ipt) &&
      (is_conflict_inherited_along_sustained_causality ipt)

    let is_preConfiguration x rCN = 
      TransitionSet.subset x (forward_transitions rCN)
      &&
      conflict_free_set x rCN

    (* This set contains only reversable transition ,so we iterate on the backwards 
    transitions to reduce the complexity *)
    let causal_bothering_set rCN = 
      TransitionSet.fold
      (fun u cb -> 
        let t = F (Transition.label u) 
        in 

        let helper = 
          TransitionSet.fold
          (fun t' cb' -> 
            if ((caused_by t' t rCN) && not(is_prevented_by u t' rCN)) then 
              TransitionSet.add t cb'
            else 
              cb'
            )
          (TransitionSet.diff (forward_transitions rCN) (TransitionSet.singleton t))
          (TransitionSet.empty)
        in

        TransitionSet.union cb (helper)
        )
      (backward_transitions rCN)
      (TransitionSet.empty)



    (* We say that a rCN is causally-respecting iff < == <<< , as a consequence, 
       it also must be that CB(rCN) == 0 *)
    let is_cause_respecting rCN = 

      (* Since <<< is defined only on forward transitions, we filter the backward ones
         from the causality relation *)
      let filtered_causality_relation = 
        CausalityRelation.filter 
        (fun {cause = t ; effect = t'} -> 
          (Transition.is_forward t) && (Transition.is_forward t')
        )
        (CN.causality_relation rCN)
      in

      CausalityRelation.equal (filtered_causality_relation) (sustained_causation rCN)
      &&
      TransitionSet.is_empty (causal_bothering_set rCN)

    

    let to_causally_respecting_net rCN = 
      let helper = 
        InhibitorSet.fold 
        (fun a ii -> match a with 
          | {source = S(s) ; target = T(t)} when (Transition.is_forward t) -> 
              if(TransitionSet.is_empty 
                  (TransitionSet.inter (postset_of_place s rCN) (causal_bothering_set rCN))
                )then 
                ii 

              else 
                InhibitorSet.add {source = S(s) ; target = T(t)} ii

          (* We don't want to remove inhibitor arcs that define causality, so 
             if t is backwards we do not want to consider its forward transition *)
          | {source = S(s) ; target = T(t)} when (Transition.is_backward t) -> 
            if(TransitionSet.is_empty 
                (TransitionSet.inter 
                  (TransitionSet.diff (postset_of_place s rCN) (TransitionSet.singleton (F (Transition.label t))))
                  (causal_bothering_set rCN))
              )then 
              ii 

            else 
              InhibitorSet.add {source = S(s) ; target = T(t)} ii

          | _ -> ii
        )
        (rCN.inhibitors)
        (InhibitorSet.empty)
      in

      let i' = 
        InhibitorSet.diff (rCN.inhibitors) helper 
      in

      {
        places = rCN.places;
        transitions = rCN.transitions;
        flow = rCN.flow;
        inhibitors = i';
        marking = rCN.marking;
      }


end;;