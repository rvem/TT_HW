open Lambda
open Hw2_unify
open Hw1_reduction

module Map = Map.Make(String);;

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

let algorithm_w = failwith "Not implemented yet";;