open Relations;;
open Sets;;
open Exceptions;;


(* We will model events like transitions *)

module PES = 
struct
  type t = {
    events : TransitionSet.t;
    causality : CausalityRelation.t;
    conflict : ConflictRelation.t;
    mutable current_configuration : TransitionSet.t;
  }

  let print p =
    print_endline "\nEvents:";
      TransitionSet.print p.events;
    
    print_endline "\nConflict:";
    ConflictRelation.print p.conflict;
  
    print_endline "\nCausality:";
    CausalityRelation.print p.causality; 

    print_endline "\nCurrent configuration: ";

    if (TransitionSet.equal (p.current_configuration) (TransitionSet.empty)) then 
      print_endline "empty"
    else
      TransitionSet.print p.current_configuration
  

  let reset_conf p =
    p.current_configuration <- TransitionSet.empty

  let correct_sets {events = ev; causality = cr; conflict = cf ; current_configuration = _} = 
    (TransitionSet.subset (CausalityRelation.causes cr) ev) &&
    (TransitionSet.subset (CausalityRelation.effects cr) ev) &&
    (TransitionSet.subset (ConflictRelation.t1s cf) ev) &&
    (TransitionSet.subset (ConflictRelation.t2s cf) ev) 


  let ipo_causality p = CausalityRelation.is_IPO (p.causality)

  let irreflexive_and_symmetric_conflict p = 
    (ConflictRelation.is_irreflexive p.conflict) && (ConflictRelation.is_symmetric p.conflict)

  let conflict_free_causality {events = ev; causality = cr; conflict = cf; current_configuration = _} = 
    TransitionSet.for_all 
    (fun e -> 
      ConflictRelation.for_all 
      (fun {t1 = e'; t2 = e''} -> 
        not (
          TransitionSet.mem e' (CausalityRelation.causes_of e cr)
            &&
          TransitionSet.mem e'' (CausalityRelation.causes_of e cr)
          )
        )
      cf)
    ev


  let is_conflict_inherited {events = _ ; causality = cr; conflict = cf; current_configuration = _} = 
    ConflictRelation.for_all 
    (fun {t1 = e ; t2 = e'} -> 
      TransitionSet.for_all 
      (fun e'' -> ConflictRelation.mem {t1 = e ; t2 = e''} cf)
      (CausalityRelation.effects_of e' cr))
    cf
  
  let is_pPES sys = 
    (correct_sets sys) &&
    (ipo_causality sys) && 
    (irreflexive_and_symmetric_conflict sys) &&
    (conflict_free_causality sys)
  
  let is_PES sys = 
    (is_pPES sys) && (is_conflict_inherited sys)

  let conflict_free_set x {events = _ ; causality = _; conflict = cf; current_configuration = _} = 
    ConflictRelation.for_all 
    (fun {t1 = e ; t2 = e'} -> 
      not(
        (TransitionSet.mem e x)
        &&
        (TransitionSet.mem e' x)
      )
      )
    cf
  
  let is_enabled_at a x p = 
    let pre_conditions = (TransitionSet.subset x p.events) && (conflict_free_set x p) && (TransitionSet.subset a p.events)
    
    in

    pre_conditions 
    &&
    ((TransitionSet.equal (TransitionSet.inter a x) (TransitionSet.empty))
      &&
    (conflict_free_set (TransitionSet.union x a) p)) 
    &&
    (TransitionSet.for_all 
      (fun e -> 
        TransitionSet.for_all
          (fun e' -> TransitionSet.mem e' x)
          (CausalityRelation.causes_of e p.causality))
      a
    )

  let fire_set a p =
    if is_enabled_at a p.current_configuration p then 
      p.current_configuration <- TransitionSet.union p.current_configuration a
    else 
      raise IllegalFireSet
  
  let fire_seq seq p = 
    List.iter
    (fun s -> fire_set s p)
    seq


  let hc p =
    let closed_conflict = 
      ConflictRelation.fold 
      (fun {t1 = e ; t2 = e'} cf -> 
        TransitionSet.fold 
        (fun e'' cf'' -> 
          if not (ConflictRelation.mem {t1 = e; t2 = e''} cf'') then 
            ConflictRelation.add {t1 = e; t2 = e''} cf''
          else 
            cf''
          )
        (CausalityRelation.effects_of e' p.causality)
        (cf))
      (p.conflict)
      (p.conflict)
    in 

    let symmetric_conflict = 
      ConflictRelation.fold 
      (fun {t1 = e' ; t2 = e} cf -> 
        if not (ConflictRelation.mem {t1 = e ; t2 = e'} cf) then 
          ConflictRelation.add {t1 = e ; t2 = e'} cf
        else 
          cf
        )
      (closed_conflict)
      (closed_conflict)
    in

    {
      events = p.events;
      causality = p.causality;
      conflict = symmetric_conflict;
      current_configuration = p.current_configuration;
    }

end;;

module ReversiblePES = 
struct
  type t = {
    events : TransitionSet.t;
    undoable_events : TransitionSet.t;
    causality : CausalityRelation.t;
    conflict : ConflictRelation.t;
    rev_causality : ReverseCausalityRelation.t;
    prevention : PreventionRelation.t;
    mutable current_configuration : Sets.TransitionSet.t;
  }

  let print p =
    print_endline "\nEvents:";
    TransitionSet.print p.events;

    print_endline "\nUndoable Events:";
    TransitionSet.print p.undoable_events;

    print_endline "\nConflict:";
    ConflictRelation.print p.conflict;
  
    print_endline "\nCausality:";
    CausalityRelation.print p.causality;

    print_endline "\nReverse Causality:";
    ReverseCausalityRelation.print p.rev_causality;

    print_endline "\nPrevention:";
    PreventionRelation.print p.prevention;

    print_endline "\nCurrent configuration:";
    if (TransitionSet.equal (p.current_configuration) (TransitionSet.empty)) then 
      print_endline "empty"
    else
      TransitionSet.print p.current_configuration


  let reset_conf p =
    p.current_configuration <- TransitionSet.empty

  let sustained_causation p = 
    CausalityRelation.fold 
    (fun {cause = e ; effect = e'} sc -> 
      if (TransitionSet.mem e p.undoable_events) then 
        if (PreventionRelation.mem {preventing = e' ; prevented = B(Transition.label e)} p.prevention) then 
          CausalityRelation.add  {cause = e ; effect = e'} sc
        else
          sc
      else 
        CausalityRelation.add {cause = e ; effect = e'} sc)
    (p.causality)
    (CausalityRelation.empty)
    
  
  let associated_pPES (p : t) : PES.t = {events = p.events ; causality = p.causality ; conflict = p.conflict ; current_configuration = p.current_configuration}

  let rev_events p = 
    TransitionSet.fold 
    (fun u tt -> TransitionSet.add (B(Transition.label u)) tt)
    p.undoable_events 
    TransitionSet.empty

  
  let correct_pes p = 
    PES.is_pPES (associated_pPES p)
  

  let correct_U p =
    (TransitionSet.subset p.undoable_events p.events) 
    && 
    (TransitionSet.equal (TransitionSet.inter (rev_events p) p.events) TransitionSet.empty)
    

  let correct_rev_causality p = 
    (ReverseCausalityRelation.is_correct p.rev_causality)
    &&
    (TransitionSet.for_all 
      (fun u -> ReverseCausalityRelation.mem {cause = u ; rev = B (Transition.label u)} p.rev_causality)
      (p.undoable_events )
    )
    &&
    (TransitionSet.for_all
      (fun u -> PES.conflict_free_set 
        (ReverseCausalityRelation.causes_of_rev 
          (B (Transition.label u)) p.rev_causality) 
        (associated_pPES p))
      (p.undoable_events)
    )
    

  let correct_prevention p = 
    (PreventionRelation.is_correct p.prevention) &&
    (PreventionRelation.for_all
      (fun {preventing = t ; prevented = t'} -> 
        not (ReverseCausalityRelation.mem {cause  = t ; rev = t'} p.rev_causality))
      (p.prevention)
    )
    

  let correct_sustained_causation p =
    (CausalityRelation.is_transitive (sustained_causation p))
    &&
    (CausalityRelation.for_all 
    (fun {cause = e ; effect = e'} -> 
      (CausalityRelation.mem {cause = e ; effect = e'} p.causality)
      &&
      (if (TransitionSet.mem e p.undoable_events) then 
        PreventionRelation.mem {preventing = e' ; prevented = B(Transition.label e)} p.prevention 
      else 
        true)
      )
    ((sustained_causation p)))
    

  let is_conflict_inherited_sustained p = 
    ConflictRelation.for_all 
    (fun {t1 = e ; t2 = e'} -> 
      TransitionSet.for_all 
      (fun e'' -> ConflictRelation.mem {t1 = e ; t2 = e''} p.conflict)
      (CausalityRelation.effects_of e' (sustained_causation p)))
    p.conflict
    
    

  let is_rPES p = 

    (correct_pes p) &&
    (correct_U p) &&
    (correct_rev_causality p) &&
    (correct_prevention p) &&
    (correct_sustained_causation p) &&
    (is_conflict_inherited_sustained p)

  let is_enabled_at a b x p =
    let f_b = 
      TransitionSet.fold
      (fun t tt-> match t with 
          B s -> TransitionSet.add (F s) tt
        | _ -> raise IllegalTransition)
      b
      TransitionSet.empty
    in

    let preconditions = 
      (is_rPES p) &&
      (TransitionSet.subset x p.events) && 
      (PES.conflict_free_set x (associated_pPES p)) &&
      (TransitionSet.subset a p.events) &&
      (TransitionSet.subset f_b p.undoable_events)

    in 

    let cond_one = 
      (TransitionSet.equal (TransitionSet.inter a x) TransitionSet.empty) &&
      (TransitionSet.subset f_b x) &&
      (PES.conflict_free_set (TransitionSet.union x a) (associated_pPES p))
    in

    let cond_two = 
      CausalityRelation.for_all 
      (fun {cause = e' ; effect = e} -> 
        if (TransitionSet.mem e a) then 
          (TransitionSet.mem e' (TransitionSet.diff x f_b))
        else
          true
        )
      (p.causality)
    in
    
    let cond_three = 
      ReverseCausalityRelation.for_all 
      (fun {cause = e' ; rev = e} -> 
        if (TransitionSet.mem (F (Transition.label e)) f_b) then 
          (TransitionSet.mem e' (TransitionSet.diff x (TransitionSet.diff f_b (TransitionSet.singleton (F (Transition.label e))))))
        else
          true
        )
      (p.rev_causality)
    in 

    let cond_four = 
      PreventionRelation.for_all 
      (fun {preventing = e' ; prevented = e} -> 
        if (TransitionSet.mem (F (Transition.label e)) f_b) then 
          not (TransitionSet.mem e' (TransitionSet.union x a))
        else
          true
        )
      (p.prevention)
    in 

    preconditions &&
    cond_one &&
    cond_two &&
    cond_three &&
    cond_four

  let exec_set a b p =
    let f_b = 
      TransitionSet.fold
      (fun t tt-> match t with 
          B s -> TransitionSet.add (F s) tt
        | _ -> raise IllegalTransition)
      b
      TransitionSet.empty
    in

    if is_enabled_at a b p.current_configuration p then 
      p.current_configuration <- TransitionSet.union (TransitionSet.diff p.current_configuration f_b) a
    else 
      raise IllegalFireSet

  let exec_seq seq p = 
    List.iter
    (fun s -> match s with 
      a,b -> exec_set a b p)
    seq

  let is_cause_respecting p = 
    CausalityRelation.equal (sustained_causation p) p.causality

  (* In this case is much easier, we simply take out all the causality tuples that are not
     tuples of the sustained causation, so we just set < to <<< *)
  let to_causally_respecting_pes p =

    {
      events = p.events;
      undoable_events = p.undoable_events;
      causality = (sustained_causation p);
      conflict = p.conflict;
      rev_causality = p.rev_causality;
      prevention = p.prevention;
      current_configuration = p.current_configuration;
    }
  
  let is_reachable_conf_CR x p = 

    (* We only consider CR rPES for now, so if a rPES is CR, then the configuration can be
       reduced to one made only of forward events *)
    if not ((TransitionSet.subset x p.events) &&
      (is_rPES p) &&
      (is_cause_respecting p)
    ) then 
      raise FailedRequirements 
    else

    if (is_cause_respecting p) then 
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
      if (PES.conflict_free_set new_x (associated_pPES p)) then 
      
        let rec helper conf enabler conf_seq = 
          let enabled_events = 
              TransitionSet.fold
            (fun t tt -> 
              if (is_enabled_at (TransitionSet.singleton t) (TransitionSet.empty) enabler p) then
                (TransitionSet.add t tt)

              else
                tt
            )
            (conf)
            (TransitionSet.empty)
          in

          if (TransitionSet.is_empty enabled_events) then
            (conf, conf_seq)
          
          else 
              let new_enabler = TransitionSet.union enabler enabled_events in 
              let new_conf = TransitionSet.diff conf enabled_events in 
              let new_conf_seq = conf_seq @ [enabled_events] in 

              (* Checking if the sequence as a whole is enabled *)
              if (is_enabled_at enabled_events (TransitionSet.empty) enabler p) then 
                helper new_conf new_enabler new_conf_seq 

              else 
                raise SequenceNotEnabled

        in

        let (remaining_events, exec_seq) = helper new_x (TransitionSet.empty) [] in 

        if (TransitionSet.is_empty remaining_events) then 
          exec_seq

        else 
          raise (NotReachable "Couldn't execute all remaining events")


      else
        raise (NotReachable "Configuration Target is not conflict free")


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

    

  
  let is_reachable_conf x p = 

    if not ((TransitionSet.subset x p.events) &&
      (is_rPES p) &&
      (PES.conflict_free_set x (associated_pPES p)) &&
      (ReverseCausalityRelation.is_causal_reversibility p.rev_causality)
    ) then 
      raise FailedRequirements 
    else


    let rec find_target ts ts_list = 
      match ts_list with 
        | [] -> raise (NotReachable "Did not find target")
        | t::l -> 
          let rev_events = 
            TransitionSet.fold 
            (fun t' tt -> 
              if (TransitionSet.mem t' p.undoable_events) then 
                TransitionSet.add (B(Transition.label t')) tt
              else
                tt
              )
            (CausalityRelation.causes_of_TransitionSet (TransitionSet.diff ts (TransitionSet.singleton t)) p.causality)
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
          
          if ((PES.conflict_free_set 
                      (TransitionSet.union 
                        (TransitionSet.singleton t)
                        (CausalityRelation.causes_of_TransitionSet (TransitionSet.diff ts (TransitionSet.singleton t)) p.causality))
                        (associated_pPES p))
                        &&
                        (not (TransitionSet.mem t (PreventionRelation.preventing_of_rev_set rev_events p.prevention)))
                      )
                  then 
                    t 

                  else
                    find_target ts l
    in 

    let rec add_causes_target causes_list target enabler seq_list = 
      match causes_list with 
        | [] when (is_enabled_at (TransitionSet.singleton target) (TransitionSet.empty) enabler p) -> 
            ((TransitionSet.add target enabler), (seq_list @ [target]))

        | t::l when (is_enabled_at (TransitionSet.singleton t) (TransitionSet.empty) enabler p) ->
            add_causes_target l target (TransitionSet.add t enabler) (seq_list @ [t])

        | _ -> raise (NotReachable "cannot add target causes")
    
    in

    let rec remove_causes_target reverse_causes_list enabler seq_list = 
      match reverse_causes_list with 
        | [] -> (enabler, seq_list)

        | t::l when (TransitionSet.mem (F(Transition.label t)) x) -> 
          remove_causes_target l enabler seq_list

        | t::l when (is_enabled_at (TransitionSet.empty) (TransitionSet.singleton t) enabler p) ->
            remove_causes_target l (TransitionSet.diff enabler (TransitionSet.singleton (F(Transition.label t)))) (seq_list @ [t])

        | _ -> raise (NotReachable "Cannot reverse causes of target")
    
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
        let cause_target_list = order_transitions_with_causality (CausalityRelation.causes_of target p.causality) p.causality in

        (* Every cause must be reversable *)
        let rec reverse_cause_target c_list r_set = match c_list with 
          | [] -> r_set
          | t::l when (TransitionSet.mem t p.undoable_events) -> reverse_cause_target l (TransitionSet.add (B(Transition.label t)) r_set)
          | _ -> raise (NotReachable "there is some target cause that is not reversible")
        in

        (* The causality closure of target must be conflict free, otherwise we cannot reach the configuration *)
        if(PES.conflict_free_set (CausalityRelation.causes_of target p.causality) (associated_pPES p)) then 
          
          (* We then update the enabler and the firing sequence after the causes of target, and target itself, have been fired *)
          let (aux_Y' , seq_list') = 
            if(TransitionSet.subset (CausalityRelation.causes_of target p.causality) aux_Y) then 
              ( (TransitionSet.add target aux_Y) , (seq_list @ [target])) 
            else
              add_causes_target cause_target_list target aux_Y seq_list
          in

          (* This is the list ordered to reverse the causes of target, since they are not part of the final configuration we need to clean them *)
          let reverse_cause_target_list = order_transitions_with_prevention (reverse_cause_target cause_target_list TransitionSet.empty) p.prevention in

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


        else
          raise (NotReachable "causes of target are in conflict")


    in

    let rec handle_sub_x_list list y seq_list = match list with 
      | [] -> (y, seq_list)
      | x::l -> 
          let (new_y, new_seq_list) = handle_sub_x (TransitionSet.empty) x y seq_list x in 

          handle_sub_x_list l new_y new_seq_list 
    in


    let sub_x_list = order_transition_sets_with_causality x (sustained_causation p) in
    let (final_y , final_seq_list) = handle_sub_x_list sub_x_list (TransitionSet.empty) [] in 

    if(TransitionSet.equal final_y x) then 
      final_seq_list

    else
      raise (NotReachable (String.concat " " ["final y is not requested configuration, y: ";
                            (TransitionSet.list_to_string (TransitionSet.elements final_y)) ;
                            " final_seq: " ; 
                            (TransitionSet.list_to_string final_seq_list);
                            " requested x: ";
                            (TransitionSet.list_to_string (TransitionSet.elements x)) ; 
                            " sub_x_list : " ; 
                            (TransitionSet.list_of_sets_to_string sub_x_list)]))



end;;