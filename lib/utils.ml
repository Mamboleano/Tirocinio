open Types
open Exceptions

let place_of_prearc = function 
  | PreArc(s, _) -> s
  | _ -> raise (TypeError "function applicable only to prearcs")
;;

let place_of_postarc = function 
  | PostArc(_, s) -> s
  | _ -> raise (TypeError "function applicable only to postarcs")
;;

let place_of_inhibarc = function 
  | InhibArc(s, _) -> s
;;

let transition_of_prearc = function 
  | PreArc(_, t) -> t
  | _ -> raise (TypeError "function applicable only to prearcs")
;;

  let transition_of_postarc = function 
  | PostArc(t, _) -> t
  | _ -> raise (TypeError "function applicable only to postarcs")
;;

let transition_of_inhibarc = function 
  | InhibArc(_, t) -> t
;;

let preset_of_place x f = match x with 
  | S(n) ->
    let f' = FlowSet.filter(fun y -> 
      (match y with 
      | PostArc(_, s) when s = S(n) -> true 
      | _ ->  false)) f in 
      
    let p = FlowSet.fold (fun x tt -> TransitionSet.add (transition_of_postarc x) tt) f' TransitionSet.empty in 
    p
;;

let preset_of_transition x f = match x with
  | T(s) -> 
    let f' = FlowSet.filter(fun y -> 
      (match y with
      | PreArc(_, t) when t = T(s) -> true
      | _ -> false)) f in 
      let p = FlowSet.fold (fun x ss -> PlaceSet.add (place_of_prearc x) ss) f' PlaceSet.empty in 
      p
;;

let postset_of_place x f = match x with
  | S(n) ->
    let f' = FlowSet.filter(fun y -> 
      (match y with 
      | PreArc(s, _) when s = S(n) -> true 
      | _ ->  false)) f in 
      
    let p = FlowSet.fold (fun x tt -> TransitionSet.add (transition_of_prearc x) tt) f' TransitionSet.empty in 
    p
;;

let postset_of_transition x f = match x with
  | T(s) -> 
    let f' = FlowSet.filter(fun y -> 
      (match y with
      | PostArc(t, _) when t = T(s) -> true
      | _ -> false)) f in 
      let p = FlowSet.fold (fun x ss -> PlaceSet.add (place_of_postarc x) ss) f' PlaceSet.empty in 
      p
;;

let inhibitors_of_transition x f = match x with 
  | T(s) -> let f' = InhibitorSet.filter(fun y -> 
    (match y with
    | InhibArc(_, t) when t = T(s) -> true
    | _ -> false)) f in

    let i = InhibitorSet.fold (fun x ss -> PlaceSet.add (place_of_inhibarc x) ss) f' PlaceSet.empty in 
    i
;;


let preset_of_TransitionSet s f = 
  TransitionSet.fold (fun t ss -> PlaceSet.union (preset_of_transition t f) ss) s PlaceSet.empty 
;;

let postset_of_TransitionSet s f = 
  TransitionSet.fold (fun t ss -> PlaceSet.union (postset_of_transition t f) ss) s PlaceSet.empty 
;;

let inhibitors_of_TransitionSet s f =
  TransitionSet.fold (fun t ss -> PlaceSet.union (inhibitors_of_transition t f) ss) s PlaceSet.empty 
;;