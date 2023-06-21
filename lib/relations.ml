open Sets;;
open Exceptions;;

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

  let causes_of_TransitionSet ts cr = 
    TransitionSet.fold 
    (fun t tt -> TransitionSet.union (causes_of t cr) tt)
    ts
    (TransitionSet.empty)
  
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

  (* This function returns the set of transitions t1 such that t1 # t2 *)
  let t1s cf = fold 
    (fun x tt-> TransitionSet.add (x.t1) tt)
    cf
    TransitionSet.empty
  (* This function returns the set of transitions t2 such that t1 # t2 *)
  let t2s cf = fold 
    (fun x tt-> TransitionSet.add (x.t2) tt)
    cf
    TransitionSet.empty

  (* This function checks if the conflict relation is irreflexive, i.e. not(a #. a) *)
  let is_irreflexive cf = TransitionSet.fold
    (fun x b -> not (mem {t1 = x ; t2 = x} cf) && b)
    (t1s cf)
    true
  
  let is_symmetric cf = fold 
  (fun {t1 = t ; t2 = t'} b -> (mem {t1 = t' ; t2 = t} cf) && b)
  cf 
  true

end;;

module ReverseCausality = 
struct

  type t = {cause : transition ; rev : transition}

  let compare = compare 

  let is_correct = function 
    | {cause = F _ ; rev = B _ } -> true 
    | _ -> false

  let cause_of = function
    | {cause = f ; rev = _ } -> f

  let rev_of = function 
  | {cause = _ ; rev = b } -> b

  let to_string {cause = t; rev = t'} = 
    (Transition.to_string t) ^ " ≺ " ^ (Transition.to_string t')
  
  let print x = print_endline (to_string x)

  
end;;

module ReverseCausalityRelation = 
struct
  include Set.Make(ReverseCausality)


  let print s = iter ReverseCausality.print s  

  let is_correct rc = 
    for_all
    (fun x -> ReverseCausality.is_correct x)
    rc

  let causes_of_rev x rc = 
    if Transition.is_backward x then
      fold
      (fun {cause = t ; rev = u} tt -> 
        if u = x then TransitionSet.add t tt 
        else tt
        )
      rc
      TransitionSet.empty
    else raise IllegalTransition 
  
  let rev_effects_of x rc = 
    if Transition.is_forward x then 
      fold
      (fun {cause = t ; rev = u} tt -> 
        if t = x then TransitionSet.add u tt 
        else tt
        )
      rc
      TransitionSet.empty
    else raise IllegalTransition 
  
  let causes rc = 
    fold
    (fun {cause = t ; rev = _} tt -> TransitionSet.add t tt)
    rc
    TransitionSet.empty

  let rev_effects rc = 
    fold
    (fun {cause = _ ; rev = u} tt -> TransitionSet.add u tt)
    rc
    TransitionSet.empty

end;;


module Prevention = 
struct

  type t = {preventing : transition ; prevented : transition}

  let compare = compare 

  let is_correct = function 
    | {preventing = F _ ; prevented = B _ } -> true 
    | _ -> false

  let preventing_of = function
    | {preventing = f ; prevented = _ } -> f

  let prevented_of = function 
  | {preventing = _ ; prevented = b } -> b

  let to_string {preventing = t; prevented = t'} = 
    (Transition.to_string t) ^ " |> " ^ (Transition.to_string t')
  
  let print x = print_endline (to_string x)

end;;

module PreventionRelation = 
struct
  include Set.Make(Prevention)


  let print s = iter Prevention.print s  

  let is_correct rc = 
    for_all
    (fun x -> Prevention.is_correct x)
    rc

  let preventing_of_rev x pr = 
    if Transition.is_backward x then
      fold
      (fun {preventing = t ; prevented = u} tt -> 
        if u = x then TransitionSet.add t tt 
        else tt
        )
      pr
      TransitionSet.empty
    else raise IllegalTransition 
  
  let prevented_of x pr = 
    if Transition.is_forward x then 
      fold
      (fun {preventing = t ; prevented = u} tt -> 
        if t = x then TransitionSet.add u tt 
        else tt
        )
        pr
      TransitionSet.empty
    else raise IllegalTransition 
  
  let preventing_ts pr = 
    fold
    (fun {preventing = t ; prevented = _} tt -> TransitionSet.add t tt)
    pr
    TransitionSet.empty

  let prevented_ts pr = 
    fold
    (fun {preventing = _ ; prevented = u} tt -> TransitionSet.add u tt)
    pr
    TransitionSet.empty

end;;
