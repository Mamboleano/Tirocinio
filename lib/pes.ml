open Relations;;
open Sets;;
open Exceptions;;


(* We will model events like transitions *)

module PrePES = 
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

    print_endline "\nCurrent configuration:";
    TransitionSet.print p.current_configuration

  let reset_conf p =
    p.current_configuration <- TransitionSet.empty

  let correct_sets {events = ev; causality = cr; conflict = cf ; current_configuration = _} = 
    (TransitionSet.subset (CausalityRelation.causes cr) ev) &&
    (TransitionSet.subset (CausalityRelation.effects cr) ev) &&
    (TransitionSet.subset (ConflictRelation.t1s cf) ev) &&
    (TransitionSet.subset (ConflictRelation.t2s cf) ev) 

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
    
  
  let associated_pPES (p : t) : PrePES.t = {events = p.events ; causality = p.causality ; conflict = p.conflict ; current_configuration = p.current_configuration}

  let rev_events p = 
    TransitionSet.fold 
    (fun u tt -> TransitionSet.add (B(Transition.label u)) tt)
    p.undoable_events 
    TransitionSet.empty

  
  let correct_pes p = 
    PrePES.is_pPES (associated_pPES p)
  

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
      (fun u -> PrePES.conflict_free_set 
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
      (PrePES.conflict_free_set x (associated_pPES p)) &&
      (TransitionSet.subset a p.events) &&
      (TransitionSet.subset f_b p.undoable_events)

    in 

    let cond_one = 
      (TransitionSet.equal (TransitionSet.inter a x) TransitionSet.empty) &&
      (TransitionSet.subset f_b x) &&
      (PrePES.conflict_free_set (TransitionSet.union x a) (associated_pPES p))
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

  let fire_set a b p =
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

  let fire_seq seq p = 
    List.iter
    (fun s -> match s with 
      a,b -> fire_set a b p)
    seq

end;;