module IPT :
  sig
    type t = {
      mutable places : Sets.PlaceSet.t;
      mutable transitions : Sets.TransitionSet.t;
      mutable flow : Sets.FlowSet.t;
      mutable inhibitors : Sets.InhibitorSet.t;
      mutable marking : Sets.PlaceSet.t;
    }
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
  end


  
module Causality :
  sig
    type t = { cause : Sets.transition; effect : Sets.transition; }
    val compare : 'a -> 'a -> int
    val cause_of : t -> Sets.transition
    val effect_of : t -> Sets.transition
    val to_string : t -> Sets.transition
    val print : t -> unit
  end

module CausalityRelation :
  sig
    type elt = Causality.t
    type t = Set.Make(Causality).t

    (* Functions inherited from Set.S *)
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val disjoint : t -> t -> bool
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val of_list : elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val to_rev_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t

    (* Defined functions *)
    val print : t -> unit
    val build : IPT.t -> t
    val causes_of : Sets.transition -> t -> Sets.TransitionSet.t
    val effects_of : Sets.transition -> t -> Sets.TransitionSet.t
    val causes : t -> Sets.TransitionSet.t
    val effects : t -> Sets.TransitionSet.t
    val is_irreflexive : t -> bool
    val is_asymmetric : t -> bool
    val is_transitive : t -> bool
    val is_IPO : t -> bool
  end



module Conflict :
  sig
    type t = { t1 : Sets.transition; t2 : Sets.transition; }
    val compare : 'a -> 'a -> int
    val to_string : t -> string
    val print : t -> unit
  end

module ConflictRelation :
  sig
    type elt = Conflict.t
    type t = Set.Make(Conflict).t

    (* Function inherited from Set.S *)
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val disjoint : t -> t -> bool
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val of_list : elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val to_rev_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t

    (* Defined functions *)
    val print : t -> unit
    val build : IPT.t -> t
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

    (* Defined functions *)
    val causality_relation : IPT.t -> CausalityRelation.t
    val conflict_relation : IPT.t -> ConflictRelation.t
    val non_flow_causality : t -> bool
    val no_backward_conflicts : t -> bool
    val no_or_causality : t -> bool
    val ipo_causality : IPT.t -> bool
    val local_conflict_freeness : t -> bool
    val correct_marking : t -> bool
    val is_pCN : IPT.t -> bool
    val is_conflict_inherited : IPT.t -> bool
    val is_CN : IPT.t -> bool
  end
