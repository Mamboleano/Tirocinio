module PES :
  sig
    type t = {
      events : Sets.TransitionSet.t;
      causality : Relations.CausalityRelation.t;
      conflict : Relations.ConflictRelation.t;
      mutable current_configuration : Sets.TransitionSet.t;
    }

    val print : t -> unit
    val reset_conf : t -> unit
    val correct_sets : t -> bool
    val ipo_causality : t -> bool
    val irreflexive_and_symmetric_conflict : t -> bool
    val conflict_free_causality : t -> bool
    val is_conflict_inherited : t -> bool
    val is_pPES : t -> bool
    val is_PES : t -> bool
    val conflict_free_set : Sets.TransitionSet.t -> t -> bool
    val is_enabled_at : Sets.TransitionSet.t -> Sets.TransitionSet.t -> t -> bool
    val fire_set : Sets.TransitionSet.t -> t -> unit
    val fire_seq : Sets.TransitionSet.t list -> t -> unit
    val hc : t -> t

  end

module ReversiblePES:
  sig
    type t = {
      events : Sets.TransitionSet.t;
      undoable_events : Sets.TransitionSet.t;
      causality : Relations.CausalityRelation.t;
      conflict : Relations.ConflictRelation.t;
      rev_causality : Relations.ReverseCausalityRelation.t;
      prevention : Relations.PreventionRelation.t;
      mutable current_configuration : Sets.TransitionSet.t;
    }

    val print : t -> unit
    val reset_conf : t -> unit
    val sustained_causation : t -> Relations.CausalityRelation.t
    val associated_pPES : t -> PES.t 
    val rev_events : t -> Sets.TransitionSet.t 
    val correct_pes : t -> bool
    val correct_U : t -> bool
    val correct_rev_causality : t -> bool
    val correct_prevention : t -> bool
    val correct_sustained_causation : t -> bool
    val is_conflict_inherited_sustained : t -> bool
    val is_rPES : t -> bool
    val is_enabled_at : Sets.TransitionSet.t -> Sets.TransitionSet.t -> Sets.TransitionSet.t -> t -> bool
    val exec_set : Sets.TransitionSet.t -> Sets.TransitionSet.t -> t -> unit
    val exec_seq : (Sets.TransitionSet.t * Sets.TransitionSet.t) list -> t -> unit
    val is_cause_respecting : t -> bool
    val to_causally_respecting_pes : t -> t
    val is_reachable_conf_CR : Sets.TransitionSet.t -> t -> Sets.TransitionSet.t list
    val order_transition_sets_with_causality : Sets.TransitionSet.t -> Relations.CausalityRelation.t -> Sets.TransitionSet.t list
    val order_transitions_with_causality : Sets.TransitionSet.t -> Relations.CausalityRelation.t -> Sets.transition list
    val order_backward_transition_sets_with_prevention : Sets.TransitionSet.t -> Relations.PreventionRelation.t -> Sets.TransitionSet.t list 
    val order_transitions_with_prevention : Sets.TransitionSet.t -> Relations.PreventionRelation.t -> Sets.transition list
    val is_reachable_conf : Sets.TransitionSet.t -> t -> Sets.transition list
  end
