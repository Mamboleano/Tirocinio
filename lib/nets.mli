module IPT :
  sig
    type t = {
      mutable places : Sets.PlaceSet.t;
      mutable transitions : Sets.TransitionSet.t;
      mutable flow : Sets.FlowSet.t;
      mutable inhibitors : Sets.InhibitorSet.t;
      mutable marking : Sets.PlaceSet.t;
    }

    val build : 
    Sets.PlaceSet.t -> Sets.TransitionSet.t ->
       Sets.FlowSet.t -> Sets.InhibitorSet.t ->
        Sets.PlaceSet.t -> t -> unit
    val valid_arc : Sets.arc -> t -> bool
    val valid_flow : t -> bool
    val print : t -> unit
    val preset_of_place : Sets.place -> t -> Sets.TransitionSet.t
    val postset_of_place : Sets.place -> t -> Sets.TransitionSet.t
    val preset_of_transition : Sets.transition -> t -> Sets.PlaceSet.t
    val postset_of_transition : Sets.transition -> t -> Sets.PlaceSet.t
    val inhibitors_of_transition : Sets.transition -> t -> Sets.PlaceSet.t
    val preset_of_TransitionSet :
      Sets.TransitionSet.t -> t -> Sets.PlaceSet.t
    val postset_of_TransitionSet :
      Sets.TransitionSet.t -> t -> Sets.PlaceSet.t
    val inhibitors_of_TransitionSet :
      Sets.TransitionSet.t -> t -> Sets.PlaceSet.t
    val is_enabled : Sets.TransitionSet.t -> t -> bool
    val fire_seq : Sets.TransitionSet.t -> t -> unit
    val caused_by : Sets.transition -> Sets.transition -> t -> bool
    val in_conflict : Sets.transition -> Sets.transition -> t -> bool
    val non_shared_inputs : Sets.Transition.t -> t -> Sets.PlaceSet.t
  end


module CN :
  sig
    type t =
      IPT.t = {
      mutable places : Sets.PlaceSet.t;
      mutable transitions : Sets.TransitionSet.t;
      mutable flow : Sets.FlowSet.t;
      mutable inhibitors : Sets.InhibitorSet.t;
      mutable marking : Sets.PlaceSet.t;
    }

    (* Function inherited from IPT *)
    val valid_arc : Sets.arc -> t -> bool
    val valid_flow : t -> bool
    val print : t -> unit
    val preset_of_place : Sets.place -> t -> Sets.TransitionSet.t
    val postset_of_place : Sets.place -> t -> Sets.TransitionSet.t
    val preset_of_transition : Sets.transition -> t -> Sets.PlaceSet.t
    val postset_of_transition : Sets.transition -> t -> Sets.PlaceSet.t
    val inhibitors_of_transition : Sets.transition -> t -> Sets.PlaceSet.t
    val preset_of_TransitionSet :
      Sets.TransitionSet.t -> t -> Sets.PlaceSet.t
    val postset_of_TransitionSet :
      Sets.TransitionSet.t -> t -> Sets.PlaceSet.t
    val inhibitors_of_TransitionSet :
      Sets.TransitionSet.t -> t -> Sets.PlaceSet.t
    val is_enabled : Sets.TransitionSet.t -> t -> bool
    val fire_seq : Sets.TransitionSet.t -> t -> unit
    val caused_by : Sets.transition -> Sets.transition -> t -> bool
    val in_conflict : Sets.transition -> Sets.transition -> t -> bool
    val non_shared_inputs : Sets.Transition.t -> t -> Sets.PlaceSet.t

    (* Defined functions *)
    val causality_relation : IPT.t -> Relations.CausalityRelation.t
    val conflict_relation : IPT.t -> Relations.ConflictRelation.t
    val conflict_free_set : Sets.TransitionSet.t -> IPT.t -> bool
    val leftclosed_causality_set : Sets.TransitionSet.t -> IPT.t -> bool
    val non_flow_causality : t -> bool
    val no_backward_conflicts : t -> bool
    val no_or_causality : t -> bool
    val ipo_causality : IPT.t -> bool
    val local_conflict_freeness : t -> bool
    val correct_marking : t -> bool
    val is_pCN : IPT.t -> bool
    val is_conflict_inherited : IPT.t -> bool
    val is_CN : IPT.t -> bool
    val is_configuration : Sets.TransitionSet.t -> IPT.t -> bool
  end


  module ReversibleCN :
  sig
    type t =
      CN.t = {
      mutable places : Sets.PlaceSet.t;
      mutable transitions : Sets.TransitionSet.t;
      mutable flow : Sets.FlowSet.t;
      mutable inhibitors : Sets.InhibitorSet.t;
      mutable marking : Sets.PlaceSet.t;
    }

    (* Function inherited from IPT *)
    val valid_arc : Sets.arc -> t -> bool
    val valid_flow : t -> bool
    val print : t -> unit
    val preset_of_place : Sets.place -> t -> Sets.TransitionSet.t
    val postset_of_place : Sets.place -> t -> Sets.TransitionSet.t
    val preset_of_transition : Sets.transition -> t -> Sets.PlaceSet.t
    val postset_of_transition : Sets.transition -> t -> Sets.PlaceSet.t
    val inhibitors_of_transition : Sets.transition -> t -> Sets.PlaceSet.t
    val preset_of_TransitionSet :
      Sets.TransitionSet.t -> t -> Sets.PlaceSet.t
    val postset_of_TransitionSet :
      Sets.TransitionSet.t -> t -> Sets.PlaceSet.t
    val inhibitors_of_TransitionSet :
      Sets.TransitionSet.t -> t -> Sets.PlaceSet.t
    val is_enabled : Sets.TransitionSet.t -> t -> bool
    val fire_seq : Sets.TransitionSet.t -> t -> unit
    val caused_by : Sets.transition -> Sets.transition -> t -> bool
    val in_conflict : Sets.transition -> Sets.transition -> t -> bool
    val non_shared_inputs : Sets.Transition.t -> t -> Sets.PlaceSet.t

    (* Functions inherited from CN *)
    val causality_relation : IPT.t -> Relations.CausalityRelation.t
    val conflict_relation : IPT.t -> Relations.ConflictRelation.t
    val conflict_free_set : Sets.TransitionSet.t -> IPT.t -> bool
    val leftclosed_causality_set : Sets.TransitionSet.t -> IPT.t -> bool
    val non_flow_causality : t -> bool
    val no_backward_conflicts : t -> bool
    val no_or_causality : t -> bool
    val ipo_causality : IPT.t -> bool
    val local_conflict_freeness : t -> bool
    val correct_marking : t -> bool
    val is_pCN : IPT.t -> bool
    val is_conflict_inherited : IPT.t -> bool
    val is_CN : IPT.t -> bool
    val is_configuration : Sets.TransitionSet.t -> IPT.t -> bool

    (* Defined functions *)
    val forward_transitions : IPT.t -> Sets.TransitionSet.t
    val backward_transitions : IPT.t -> Sets.TransitionSet.t
    val sustained_causation : t -> Relations.CausalityRelation.t
    val reverses_of : Sets.transition -> t -> Sets.TransitionSet.t
    val sustainly_caused_by : Sets.transition -> Sets.transition -> IPT.t -> bool
    val forward_subnet_is_pCN : IPT.t -> bool
    val exaclty_one_reverse_transition : IPT.t -> bool
    val not_caused_and_prevented : IPT.t -> bool
    val is_conflict_inherited_along_sustained_causality : IPT.t -> bool
    val is_rCN : IPT.t -> bool
    val is_preConfiguration : Sets.TransitionSet.t -> IPT.t -> bool
  end
