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
        mutable initial_marking : PlaceSet.t;
        mutable current_marking : PlaceSet.t;
      }

      let build s t f i m net = 
        net.places <- s ;
        net.transitions <- t;
        net.flow <- f;
        net.inhibitors <- i;
        net.initial_marking <- m;
        net.current_marking <- m
      
      let reset_marking ipt =
        ipt.current_marking <- ipt.initial_marking

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
    
      print_endline "\nInitial marking:";
      PlaceSet.print ipt.initial_marking

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
    
      (PlaceSet.subset preset_a ipt.current_marking)
      &&
      (PlaceSet.for_all (fun x ->  not (PlaceSet.mem x ipt.current_marking) && not (PlaceSet.mem x postset_a)) inhibset_a)
    
    
    (* Given a set of transitions a, this function fires a at the current marking of ipt,
        only if a is enabled, and if so, it then modifies the marking of the ipt*)
    let fire_seq a ipt =
      let preset_a = preset_of_TransitionSet a ipt in
      let postset_a = postset_of_TransitionSet a ipt in

      if is_enabled a ipt then
        ipt.current_marking <- PlaceSet.union (PlaceSet.diff ipt.current_marking preset_a) postset_a


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
    let b1 = PlaceSet.equal (preset_of_TransitionSet ipt.transitions ipt) ipt.initial_marking in
    let b2 = PlaceSet.subset (inhibitors_of_TransitionSet ipt.transitions ipt) ipt.initial_marking in

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

  let reverse_causality_relation ipt = 
    TransitionSet.fold 
    (fun t rc -> 

        ReverseCausalityRelation.union
        (TransitionSet.fold 
        (fun t' rc' -> 

          if(PlaceSet.equal 
            (PlaceSet.inter (preset_of_transition t ipt) (inhibitors_of_transition t' ipt)) 
            (PlaceSet.empty)) then 
              rc'
          else
            ReverseCausalityRelation.add {cause = t; rev = t'} rc'
          
        
        ) 
        (backward_transitions ipt)
        (ReverseCausalityRelation.empty))
        rc
    )
    (forward_transitions ipt)
    (ReverseCausalityRelation.empty)

  let prevention_relation ipt = 
    TransitionSet.fold 
    (fun t pr -> 

        PreventionRelation.union
        (TransitionSet.fold 
        (fun t' pr' -> 

          if(PlaceSet.equal 
            (PlaceSet.inter (postset_of_transition t ipt) (inhibitors_of_transition t' ipt)) 
            (PlaceSet.empty)) then 
              pr'
          else
            PreventionRelation.add {preventing = t; prevented = t'} pr'
          
        
        ) 
        (backward_transitions ipt)
        (PreventionRelation.empty))
        pr
    )
    (forward_transitions ipt)
    (PreventionRelation.empty)
  
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
       initial_marking = ipt.initial_marking;
       current_marking = ipt.initial_marking
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

  (* Given a set of forward transitions a and a set of backwards transitions b, this function returns true iff (a U b) is enabled at the current marking of the ipt *)
  let is_enabled_at a b m ipt =
    let preset_ab = preset_of_TransitionSet (TransitionSet.union a b) ipt in
    let postset_a = postset_of_TransitionSet a ipt in
    let inhibset_ab = inhibitors_of_TransitionSet (TransitionSet.union a b) ipt in 
  
    (PlaceSet.subset preset_ab m)
    &&
    (PlaceSet.for_all 
    (fun x ->  not (PlaceSet.mem x m) && not (PlaceSet.mem x postset_a))
     inhibset_ab
    )

  let fire_set a b ipt = 
    if (is_enabled_at a b ipt.current_marking ipt) then 
      let new_m = PlaceSet.union 
                  (PlaceSet.diff 
                    (PlaceSet.union 
                      (PlaceSet.diff ipt.current_marking (preset_of_TransitionSet a ipt))
                      (postset_of_TransitionSet a ipt)) 
                    (preset_of_TransitionSet b ipt)) 
                  (postset_of_TransitionSet b ipt)
      in

      ipt.current_marking <- new_m

    else
      raise SequenceNotEnabled
  
  let fire_seq seq c = 
    List.iter
    (fun s -> match s with 
      a,b -> fire_set a b c)
    seq
    

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
      initial_marking = rCN.initial_marking;
      current_marking = rCN.initial_marking
    }
  
  let is_reachable_conf_CR x v = 
    (* We only consider CR rCN for now, so if a rCN is CR, then the configuration can be
        reduced to one made only by forward transitions *)
    if (is_cause_respecting v) then 
      let new_x = 
        TransitionSet.fold
        (fun t xx -> match t with 
          | B s -> TransitionSet.diff xx (TransitionSet.of_list [B s ; F s])
          | _ -> xx)
        (x)
        (x)
      in

      (* Here we first check that the configuration is conflict free, otherwise,
          it would not be reachable *)
      if (conflict_free_set new_x v) then 
      
        let rec helper conf enabler conf_seq = 
          let enabled_transitions = 
              TransitionSet.fold
            (fun t tt -> 
              if (is_enabled_at (TransitionSet.singleton t) (TransitionSet.empty) enabler v) then
                (TransitionSet.add t tt)

              else
                tt
            )
            (conf)
            (TransitionSet.empty)
          in

          if (TransitionSet.is_empty enabled_transitions) then
            (conf, conf_seq)
          
          else 
              let new_enabler = 
                PlaceSet.union 
                  (postset_of_TransitionSet enabled_transitions v)
                  (PlaceSet.diff enabler (preset_of_TransitionSet enabled_transitions v))
                in 
              let new_conf = TransitionSet.diff conf enabled_transitions in 
              let new_conf_seq = conf_seq @ [enabled_transitions] in 

              (* Checking if the sequence as a whole is enabled *)
              if (is_enabled_at enabled_transitions (TransitionSet.empty) enabler v) then 
                helper new_conf new_enabler new_conf_seq 

              else 
                raise SequenceNotEnabled

        in

        let (remaining_events, firing_seq) = helper new_x (v.initial_marking) [] in 

        if (TransitionSet.is_empty remaining_events) then 
          firing_seq

        else 
          raise (NotReachable "Could not fire all the remaining transitions")


      else
        raise (NotReachable "Condiguration target is not conflict free")


    else
      raise NonCauseRespecting;;

  
  let order_transition_sets_with_causality ts c_relation = 
    let aux remaining_transitions enabler cr = 
      TransitionSet.fold 
      (fun t tt -> 
        if (TransitionSet.subset (CausalityRelation.causes_of t cr) enabler) then 
          TransitionSet.add t tt 
        else
          tt
      )
      (remaining_transitions)
      (TransitionSet.empty)
    in

    let rec helper remaining_transitions enabler cr ordered_list = 

      let enabled_transitions = aux remaining_transitions enabler cr 
    
      in 

      if(TransitionSet.equal enabled_transitions (TransitionSet.empty)) then 
        ordered_list 

      else
        helper 
              (TransitionSet.diff remaining_transitions enabled_transitions)
              (TransitionSet.union enabler enabled_transitions)
              cr 
              (ordered_list @ [enabled_transitions])

    in

    helper ts (TransitionSet.empty) c_relation []


  let order_transitions_with_causality ts c_relation = 
    let list_of_sets = order_transition_sets_with_causality ts c_relation in 

    let rec aux l_sets l_trans= match l_sets with 
      | [] -> l_trans
      | t::l -> aux l (l_trans @ TransitionSet.elements t)

    in
    aux list_of_sets []
  
  
  let order_backward_transition_sets_with_prevention ts prevention = 
    let aux remaining_transitions conf pr = 
      TransitionSet.fold 
      (fun t tt -> 
        if (TransitionSet.equal 
              (TransitionSet.inter 
                (conf)
                (PreventionRelation.preventing_of_rev t pr))
              (TransitionSet.empty)
            ) then 
          TransitionSet.add t tt 
        else
          tt
      )
      (remaining_transitions)
      (TransitionSet.empty)
    in

    let rec helper remaining_transitions conf pr ordered_list = 

      let undoable_transitions = aux remaining_transitions conf pr 
    
      in 

      if(TransitionSet.equal undoable_transitions (TransitionSet.empty)) then 
        ordered_list 

      else
        let reversed_transitions = 
          TransitionSet.fold
          (fun t tt -> TransitionSet.add (F(Transition.label t)) tt)
          (undoable_transitions)
          (TransitionSet.empty)
        in

        helper 
              (TransitionSet.diff remaining_transitions undoable_transitions)
              (TransitionSet.diff conf reversed_transitions)
              pr 
              (ordered_list @ [undoable_transitions])

    in

    helper ts ts prevention []


  let order_transitions_with_prevention ts pr = 
    let list_of_sets = order_backward_transition_sets_with_prevention ts pr in 

    let rec aux l_sets l_trans= match l_sets with 
      | [] -> l_trans
      | t::l -> aux l (l_trans @ TransitionSet.elements t)

    in
    aux list_of_sets []
  

  let is_reachable_conf x c = 

    if not ((TransitionSet.subset x c.transitions) &&
      (is_rCN c) &&
      (conflict_free_set x c) &&
      (ReverseCausalityRelation.is_causal_reversibility (reverse_causality_relation c))
    ) then (
      reset_marking c;
      raise FailedRequirements 
    )
      
    else

    let update_marking t = 
      c.current_marking <- PlaceSet.union (PlaceSet.diff c.current_marking (preset_of_transition t c)) (postset_of_transition t c)
    in

    let rec find_target ts ts_list = 
      match ts_list with 
        | [] ->
            (reset_marking c;
            raise (NotReachable "Did not find target"))
        | t::l -> 
          let rev_events = 
            TransitionSet.fold 
            (fun t' tt -> 
              if (TransitionSet.mem (B(Transition.label t')) (backward_transitions c)) then 
                TransitionSet.add (B(Transition.label t')) tt
              else
                tt
              )
            (CausalityRelation.causes_of_TransitionSet (TransitionSet.diff ts (TransitionSet.singleton t)) (causality_relation c))
            (TransitionSet.empty)
          in

          (*
          print_endline (String.concat " " [
        "Rev events : " ; 
        (TransitionSet.list_to_string (TransitionSet.elements rev_events)) ; 
        "current transition : " ; 
        (Transition.to_string t);
        "preventing set : ";
        (TransitionSet.list_to_string (TransitionSet.elements (PreventionRelation.preventing_of_rev_set rev_events p.prevention)))
        
      ]);*)
          
          if ((CN.conflict_free_set 
                      (TransitionSet.union 
                        (TransitionSet.singleton t)
                        (CausalityRelation.causes_of_TransitionSet (TransitionSet.diff ts (TransitionSet.singleton t)) (causality_relation c)))
                        c)
                        &&
                        (not (TransitionSet.mem t (PreventionRelation.preventing_of_rev_set rev_events (prevention_relation c))))
                      )
                  then 
                    t 

                  else
                    find_target ts l
    in 

    let rec add_causes_target causes_list target enabler seq_list = 
      match causes_list with 
        | [] when (is_enabled_at (TransitionSet.singleton target) (TransitionSet.empty) c.current_marking c) -> 
            update_marking target;
            ((TransitionSet.add target enabler), (seq_list @ [target]))

        | t::l when (is_enabled_at (TransitionSet.singleton t) (TransitionSet.empty) c.current_marking c) ->
            update_marking t;
            add_causes_target l target (TransitionSet.add t enabler) (seq_list @ [t])

        | _ -> 
            (reset_marking c;
            raise (NotReachable "cannot add target causes"))
    
    in

    let rec remove_causes_target reverse_causes_list enabler seq_list = 
      match reverse_causes_list with 
        | [] -> (enabler, seq_list)

        | t::l when (TransitionSet.mem (F(Transition.label t)) x) -> 
          remove_causes_target l enabler seq_list

        | t::l when (is_enabled_at (TransitionSet.empty) (TransitionSet.singleton t) c.current_marking c) ->
            update_marking t;
            remove_causes_target l (TransitionSet.diff enabler (TransitionSet.singleton (F(Transition.label t)))) (seq_list @ [t])

        | _ -> 
            (reset_marking c;
            raise (NotReachable "Cannot reverse causes of target"))
    
    in

    let rec handle_sub_x executed_transitions remaining_transitions aux_Y seq_list sub_x = 

      (* 
      DEBUG PRINTS
      

      print_endline (String.concat " " [
        "Chiamata handle_sub_x : executed_transitions : " ; 
        (TransitionSet.list_to_string (TransitionSet.elements executed_transitions)) ; 
        "remaining_transitions : " ; 
        (TransitionSet.list_to_string (TransitionSet.elements remaining_transitions)) ;
        "aux_Y : " ; 
        (TransitionSet.list_to_string (TransitionSet.elements aux_Y)) ;
      ]);
      *)

      

      if((TransitionSet.equal remaining_transitions (TransitionSet.empty)) && (TransitionSet.equal executed_transitions sub_x)) then 
        (aux_Y , seq_list)

      else 
        
        (* First we find the target transition to fire *)
        let target = find_target remaining_transitions (TransitionSet.elements remaining_transitions) in 
        
        (* Then we find the causes of such target that we need to add in order to fire it *)
        let cause_target_list = order_transitions_with_causality (CausalityRelation.causes_of target (causality_relation c)) (causality_relation c) in

        (* Every cause must be reversable *)
        let rec reverse_cause_target c_list r_set = match c_list with 
          | [] -> r_set
          | t::l when (TransitionSet.mem (B(Transition.label t)) (backward_transitions c)) -> reverse_cause_target l (TransitionSet.add (B(Transition.label t)) r_set)
          | _ -> 
            ( 
              reset_marking c;
              raise (NotReachable "there is some target cause that is not reversible")
            )
            
        in

        (* The causality closure of target must be conflict free, otherwise we cannot reach the configuration *)
        if(CN.conflict_free_set (CausalityRelation.causes_of target (causality_relation c)) c) then 
          
          (* We then update the enabler and the firing sequence after the causes of target, and target itself, have been fired *)
          let (aux_Y' , seq_list') = 
            if(TransitionSet.subset (CausalityRelation.causes_of target (causality_relation c)) aux_Y) then 
              ( (TransitionSet.add target aux_Y) , (seq_list @ [target])) 
            else
              add_causes_target cause_target_list target aux_Y seq_list
          in

          (* This is the list ordered to reverse the causes of target, since they are not part of the final configuration we need to clean them *)
          let reverse_cause_target_list = order_transitions_with_prevention (reverse_cause_target cause_target_list TransitionSet.empty) (prevention_relation c) in

          (* 
          DEBUG PRINTS

          print_endline (String.concat " " [
          "causes_of_target : " ; 
          (TransitionSet.list_to_string cause_target_list) ;
          "reverse_causes_of_target : " ; 
          (TransitionSet.list_to_string reverse_cause_target_list) ;
          "aux_Y' : " ; 
          (TransitionSet.list_to_string (TransitionSet.elements aux_Y')) ;
          ]);
          *)
          
          (* At this point the aux_Y should contain only the target in addition to before *)
          let (aux_Y'' , seq_list'') = remove_causes_target reverse_cause_target_list aux_Y' seq_list' in
          let new_executed_transitions = TransitionSet.add target executed_transitions in 
          let new_remaining_transitions = TransitionSet.diff remaining_transitions (TransitionSet.singleton target) in

          handle_sub_x new_executed_transitions new_remaining_transitions aux_Y'' seq_list'' sub_x


        else(
          reset_marking c;
          raise (NotReachable "causes of target are in conflict"))


    in

    let rec handle_sub_x_list list y seq_list = match list with 
      | [] -> (y, seq_list)
      | x::l -> 
          let (new_y, new_seq_list) = handle_sub_x (TransitionSet.empty) x y seq_list x in 

          handle_sub_x_list l new_y new_seq_list 
    in


    let sub_x_list = order_transition_sets_with_causality x (sustained_causation c) in
    let (final_y , final_seq_list) = handle_sub_x_list sub_x_list (TransitionSet.empty) [] in 

    if(TransitionSet.equal final_y x) then 
      (reset_marking c;
      final_seq_list)

    else
      (reset_marking c;
      raise (NotReachable (String.concat " " ["final y is not requested configuration, y: ";
                            (TransitionSet.list_to_string (TransitionSet.elements final_y)) ;
                            " final_seq: " ; 
                            (TransitionSet.list_to_string final_seq_list);
                            " requested x: ";
                            (TransitionSet.list_to_string (TransitionSet.elements x)) ; 
                            " sub_x_list : " ; 
                            (TransitionSet.list_of_sets_to_string sub_x_list)])))
    


end;;