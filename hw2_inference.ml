open Lambda
open Hw2_unify
open Hw1_reduction

module Map = Map.Make(String);;
module Set = Set.Make(String);;

type simp_type 
  = S_Elem of string 
	| S_Arrow of simp_type * simp_type;;

type hm_lambda 
  = HM_Var of string 
	| HM_Abs of string * hm_lambda 
	| HM_App of hm_lambda * hm_lambda 
	| HM_Let of string * hm_lambda * hm_lambda;;

type hm_type 
  = HM_Elem of string 
	| HM_Arrow of hm_type * hm_type 
	| HM_ForAll of string * hm_type;;

let type_cnt = ref 0;;

let next_type () = 
  let name = "t" ^ string_of_int !type_cnt in
  type_cnt := !type_cnt + 1;
  name
;;

exception Unknown_term_exception;;

let infer_simp_type lambda = 
  let rec type_term term = match term with
    | Var x -> S_Elem x
    | Fun (f, l::r::[]) -> S_Arrow (type_term l, type_term r)
    | _ -> raise Unknown_term_exception in
  let rec term_type t = match t with
    | S_Elem x -> Hw2_unify.Var x
    | S_Arrow (l, r) -> 
      Hw2_unify.Fun ("arrow", [(term_type l);(term_type r)]) in
  let rec type_vars l map = match l with
    | [] -> map
    | (h::t) -> type_vars t (Map.add h (S_Elem (next_type ())) map) in
  let rec get_system l map = match l with
    | Lambda.Var x -> ([], Map.find x map)
    | Lambda.Abs (x, l) ->
      let new_map = Map.add x (S_Elem (next_type ())) map in
      let new_system, new_type = get_system l new_map in
      (new_system, S_Arrow(Map.find x new_map, new_type))
    | Lambda.App (l1, l2) -> 
      let (new_system1, new_type1) = get_system l1 map in
      let (new_system2, new_type2) = get_system l2 map in
      let new_type = S_Elem (next_type ()) in
      (new_system1 @ new_system2 @ [(new_type1, S_Arrow (new_type2, new_type))], new_type) in
  let system, t = get_system lambda (type_vars (free_vars lambda) Map.empty) in
  try 
    let res = solve_system (List.map (fun (a, b) -> (term_type a, term_type b)) system) in
    match res with
      | Some solution -> Some (List.map (fun (a, b) -> (a, type_term b)) solution, type_term (apply_substitution solution (term_type t)))
      | None -> None
  with
    Unknown_term_exception -> None
;;

exception Type_exception;;

let algorithm_w hm_lmd = 
  let rec subst substitute hm_type set = match hm_type with
    | HM_Elem a -> 
      if Set.mem a set then 
        hm_type 
      else if Map.mem a substitute then 
        Map.find a substitute 
      else 
        hm_type
    | HM_Arrow (a, b) -> HM_Arrow (subst substitute a set, subst substitute b set)
    | HM_ForAll (a, b) -> HM_ForAll (a, subst substitute b (Set.add a set)) in
  let free_types hm_type = 
    let rec free_types_impl hm_type set = match hm_type with
      | HM_Elem (a) -> if Set.mem a set then Set.empty else Set.singleton a
      | HM_Arrow (a, b) -> Set.union (free_types_impl a set) (free_types_impl b set)
      | HM_ForAll (a, b) -> free_types_impl b (Set.add a set)
    in free_types_impl hm_type Set.empty in
  let free_vars_hm hm_lambda = 
    let rec free_vars_hm_impl hm_lambda set = match hm_lambda with
      | HM_Var (a) -> if Set.mem a set then Set.empty else Set.singleton a
      | HM_App (a, b) -> Set.union (free_vars_hm_impl a set) (free_vars_hm_impl b set)
      | HM_Abs (a, b) -> free_vars_hm_impl b (Set.add a set)
      | HM_Let (a, b, c) -> Set.union (free_vars_hm_impl b set) (free_vars_hm_impl c (Set.add a set))
    in free_vars_hm_impl hm_lambda Set.empty in
  let rec rm_union hm_type = match hm_type with
    | HM_ForAll (a, b) -> subst (Map.add a (HM_Elem (next_type ())) Map.empty) (rm_union b) Set.empty
    | _ -> hm_type in
  let add_union hm_type types = 
    let availble_types = Map.fold (fun a b set -> Set.union (free_types b) set) types Set.empty in
    let f = fun a b -> if Set.mem a availble_types then b else Set.add a b in
    Set.fold (fun a b -> HM_ForAll (a, b)) (Set.fold f (free_types hm_type) Set.empty) hm_type in
  let subst_types substitute types = 
    Map.fold (fun a b map -> Map.add a (subst substitute b Set.empty) map) types Map.empty in
  let rec term_of_hm_type hm_type = match hm_type with
    | HM_Elem a -> Hw2_unify.Var a
    | HM_Arrow (a, b) -> Hw2_unify.Fun ("arrow", [(term_of_hm_type a);(term_of_hm_type b)])
    | _ -> raise Type_exception in
  let rec hm_type_of_term term = match term with
    | Hw2_unify.Var a -> HM_Elem a 
    | Hw2_unify.Fun (name, [l;r]) -> HM_Arrow (hm_type_of_term l, hm_type_of_term r)
    | _ -> raise Type_exception in
  let combine_subst subst1 subst2 = 
    Map.fold (fun a b map -> if Map.mem a map then map else Map.add a b map) subst1
      (Map.fold (fun a b map -> Map.add a (subst subst1 b Set.empty) map) subst2 Map.empty) in
  let rec algorithm_w_impl hm_lambda types = match hm_lambda with
    | HM_Var x  -> 
      if Map.mem x types then 
        (rm_union (Map.find x types), Map.empty) 
      else 
        raise Type_exception
    | HM_App (e1, e2) ->
      (let (hmt1, t1) = algorithm_w_impl e1 types in
      let (hmt2, t2) = algorithm_w_impl e2 (subst_types t1 types) in
      let new_type = HM_Elem (next_type ()) in
      match solve_system ([((term_of_hm_type (subst t2 hmt1 Set.empty)), (term_of_hm_type (HM_Arrow(hmt2, new_type))))]) with
        | None -> raise Type_exception
        | Some ans -> let ans_types = combine_subst (List.fold_left (fun map (str, term) -> Map.add str (hm_type_of_term term) map) Map.empty ans) (combine_subst t2 t1) in
          (subst ans_types new_type Set.empty, ans_types)
      )
    | HM_Abs (x, e) -> 
      let new_type = HM_Elem (next_type ()) in
      let (hmt1, t1) = algorithm_w_impl e (Map.add x new_type (Map.remove x types)) in
      (HM_Arrow(subst t1 new_type Set.empty, hmt1), t1)
    | HM_Let (x, e1, e2) -> let (hmt1, t1) = algorithm_w_impl e1 types in
      let new_types = subst_types t1 types in
      let (hmt2, t2) = algorithm_w_impl e2 (Map.add x (add_union hmt1 new_types) (Map.remove x new_types)) in
      (hmt2, combine_subst t2 t1) in
  let types = Set.fold (fun a map -> Map.add a (HM_Elem (next_type ())) map) (free_vars_hm hm_lmd) (Map.empty) in
  try
    let (tp, map) = algorithm_w_impl hm_lmd types in Some (Map.bindings map, tp)
  with 
    Type_exception -> None
;;