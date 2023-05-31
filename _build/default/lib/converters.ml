open Nets;;
open Relations;;
open Sets;;
open Pes;;
open Exceptions;;


let to_pPES (net : CN.t) : PrePES.t = 
  let new_conf = 
    ConflictRelation.filter
    (fun {t1 = t ; t2 = t'} -> not (t = t'))
    (CN.conflict_relation net)
  in

  {
    events = net.transitions;
    causality = (CN.causality_relation net);
    conflict = new_conf;
    current_configuration = TransitionSet.empty
  }
;;

let to_rPES (net : ReversibleCN.t) : ReversiblePES.t = 
  let e = TransitionSet.diff (net.transitions) (ReversibleCN.backward_transitions net) in 

  let u = 
    TransitionSet.filter
    (fun t -> match t with 
      | F s -> 
        (TransitionSet.mem (B s) (ReversibleCN.backward_transitions net))
        &&
        (PlaceSet.equal (ReversibleCN.preset_of_transition (F s) net) (ReversibleCN.postset_of_transition (B s) net))

      | _ -> raise IllegalTransition)
    (ReversibleCN.forward_transitions net)

  in 

  let cr = 
    CausalityRelation.filter 
    (fun {cause = t' ; effect = t} -> (TransitionSet.mem t' e) && (TransitionSet.mem t e))
    (ReversibleCN.causality_relation net)
  in 

  let cf = 
    ConflictRelation.filter 
    (fun {t1 = t' ; t2 = t} -> (TransitionSet.mem t' e) && (TransitionSet.mem t e) && not(t = t'))
    (ReversibleCN.conflict_relation net)
  in 

  let rev_cr =
    let helper t = 
      TransitionSet.fold 
      (fun t' tt -> 
        if (PlaceSet.equal 
              (PlaceSet.inter 
                (ReversibleCN.preset_of_transition t net) 
                (ReversibleCN.inhibitors_of_transition t' net)
              ) 
              (PlaceSet.empty)
            ) then 
          tt 
        else
          ReverseCausalityRelation.add {cause = t ; rev = t'} tt
          )
      (ReversibleCN.backward_transitions net)
      (ReverseCausalityRelation.empty)
    in



    TransitionSet.fold 
    (fun ev rc -> ReverseCausalityRelation.union (helper ev) rc)
    e 
    (ReverseCausalityRelation.empty)
  in 

  let prev =
    let helper t = 
      TransitionSet.fold 
      (fun t' tt -> 
        if (PlaceSet.equal 
              (PlaceSet.inter 
                (ReversibleCN.postset_of_transition t net) 
                (ReversibleCN.inhibitors_of_transition t' net)
              ) 
              (PlaceSet.empty)
            ) then 
          tt 
        else
          PreventionRelation.add {preventing = t ; prevented = t'} tt
          )
      (ReversibleCN.backward_transitions net)
      (PreventionRelation.empty)
    in



    TransitionSet.fold 
    (fun ev rc -> PreventionRelation.union (helper ev) rc)
    e 
    (PreventionRelation.empty)
  in 

  {
    events = e ;
    undoable_events = u;
    causality = cr;
    conflict = cf;
    rev_causality = rev_cr;
    prevention = prev;
    current_configuration = TransitionSet.empty;
  }

let to_pCN (p : PrePES.t) : CN.t = 
  let s = 
    let pre_post_places = 
      TransitionSet.fold 
      (fun e ss -> PlaceSet.union (PlaceSet.of_list [PrePlace(e) ; PostPlace(e)]) ss)
      (p.events)
      (PlaceSet.empty)
      in 

    let conf_places = 
      ConflictRelation.fold 
      (fun {t1 = e ; t2 = e'} ss -> 
        if PlaceSet.mem (ConfPlace(e', e)) ss then 
          ss 
        else 
          PlaceSet.add (ConfPlace(e, e')) ss)
      (p.conflict)
      (PlaceSet.empty)
      in 
    
    PlaceSet.union pre_post_places conf_places
    in 

  let f = 
    PlaceSet.fold 
    (fun plc ff -> match plc with 
      | PrePlace e -> FlowSet.add {source = S(plc) ; target = T(e)} ff
      | PostPlace e -> FlowSet.add {source = T(e) ; target = S(plc)} ff
      | ConfPlace (e, e') -> 
          FlowSet.union 
          (FlowSet.of_list [{source = S(plc) ; target = T(e)} ; {source = S(plc) ; target = T(e')}])
          ff
      | _ -> raise IllegalArc)
    (s)
    (FlowSet.empty)
    in

    let i = 
      CausalityRelation.fold 
      (fun {cause = e' ; effect = e} ii -> InhibitorSet.add {source = S(PrePlace(e')) ; target = T(e)} ii)
      (p.causality)
      (InhibitorSet.empty)
    in

    let m = 
      PlaceSet.fold 
      (fun plc mm -> match plc with 
        | I(_) -> raise IllegalPlace
        | PostPlace _ -> mm
        | _ -> PlaceSet.add plc mm)
      (s)
      (PlaceSet.empty)
    in

    {
      places = s;
      transitions = p.events;
      flow = f;
      inhibitors = i;
      marking = m;
    }

let to_rCN (p : ReversiblePES.t) : ReversibleCN.t = 
  let pcn = to_pCN 
    {events = p.events ;
     causality = p.causality ;
     conflict = p.conflict ;
     current_configuration = TransitionSet.empty
     }

   in 

  let t = 
    TransitionSet.union 
      (pcn.transitions)
      (TransitionSet.fold 
        (fun u tt -> match u with 
          | F s -> TransitionSet.add (B s) tt
          | B _ -> raise IllegalTransition)
        (p.undoable_events)
        (TransitionSet.empty)
      )
  in

  let f = 
    let preset_flows u = 
      PlaceSet.fold 
      (fun plc ss -> FlowSet.add {source = S(plc) ; target = T(B(Transition.label u))} ss)
      (IPT.postset_of_transition u pcn)
      (FlowSet.empty)
    in

    let postset_flows u = 
      PlaceSet.fold 
      (fun plc ss -> FlowSet.add {source = T(B(Transition.label u)) ; target = S(plc)} ss)
      (IPT.preset_of_transition u pcn)
      (FlowSet.empty)
    in

    FlowSet.union 
    (pcn.flow)
    (TransitionSet.fold 
      (fun u ff -> match u with 
        | F _ -> FlowSet.union (FlowSet.union (preset_flows u) (postset_flows u)) ff
        | B _ -> raise IllegalTransition)
      (p.undoable_events)
      (FlowSet.empty)
      )
  in

  let i = 
    let inhibs_from_reverse_causality = 
      ReverseCausalityRelation.fold 
      (fun {cause = e ; rev = ru} ii -> 
        InhibitorSet.add {source = S(PrePlace(e)) ; target = T(ru)} ii)
      (p.rev_causality)
      (InhibitorSet.empty)
    in

    let inhibs_from_prevention = 
      PreventionRelation.fold 
      (fun {preventing = e ; prevented = ru} ii -> 
        InhibitorSet.add {source = S(PostPlace(e)) ; target = T(ru)} ii)
      (p.prevention)
      (InhibitorSet.empty)
    in
  
  InhibitorSet.union 
    (InhibitorSet.union inhibs_from_reverse_causality inhibs_from_prevention)
    (pcn.inhibitors)
  in

  {
    places = pcn.places ;
    transitions = t ;
    flow = f ;
    inhibitors = i ;
    marking = pcn.marking ;
  }