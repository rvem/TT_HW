open Lambda

module Set = Set.Make (String);;
module Map = Map.Make (String);;

let free_vars lambda_expr = 
	let rec find expr locked = match expr with
  	| Var x -> if Set.mem x locked then 
                 Set.empty
               else
                 Set.singleton x
  	| Abs (x, l) -> find l (Set.add x locked)
    | App (l1, l2) -> Set.union (find l1 locked) (find l2 locked) in
  Set.elements (find lambda_expr Set.empty)
;;

let rec free_to_subst substitute expr var =
  match expr with
    | Var x -> true
    | Abs (x, l) -> 
      ((not (List.mem x (free_vars substitute))) && free_to_subst substitute l var) || x = var
    | App (l1, l2) -> free_to_subst substitute l1 var && free_to_subst substitute l2 var
;;

let rec is_normal_form lambda_expr = match lambda_expr with
  | Var x -> true
  | App (Abs (x, l1), l2) -> false
  | App (l1, l2) -> is_normal_form l1 && is_normal_form l2
  | Abs (x, l) -> is_normal_form l
;;

let cnt = ref 0;;

let next_name = 
  let name = "x" ^ string_of_int !cnt in
    cnt := !cnt + 1;
    name
;;

let rec subst substitute expr var = match expr with
    | Var x -> if x = var then substitute else expr
    | Abs (x, l) -> if x = var then expr else Abs (x, subst substitute l var)
    | App (l1, l2) -> App (subst substitute l1 var, subst substitute l2 var)
;;

let rec is_alpha_equivalent lambda_expr1 lambda_expr2 = 
  match (lambda_expr1, lambda_expr2) with
  | (Var x, Var y) -> x = y
  | (Abs (x1, l1), Abs (x2, l2)) -> 
    let tmp = Var (next_name) in 
      is_alpha_equivalent (subst tmp l1 x1) (subst tmp l2 x2)
  | (App (l11, l12), App (l21, l22)) -> is_alpha_equivalent l11 l21 && is_alpha_equivalent l12 l22
  | _ -> false
;;

let rec normal_beta_reduction lambda_expr = 
  if is_normal_form lambda_expr then
    lambda_expr
  else
    match lambda_expr with
      | Var x -> lambda_expr
      | Abs (x, l) -> Abs (x, normal_beta_reduction l)
      | App (Abs (x, l1), l2) -> subst l2 l1 x
      | App (l1, l2) -> 
        if is_normal_form l1 then
          App (l1, normal_beta_reduction l2)
        else
          App (normal_beta_reduction l1, l2)
;;

let reduce_to_normal_form = failwith "Not implemented yet";;