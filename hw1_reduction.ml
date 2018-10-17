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

let next_name () = 
  let name = "sample_name" ^ string_of_int !cnt in
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
    let tmp = Var (next_name ()) in
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

type lambda_reference 
  = Var_ref of string
  | Abs_ref of string * lambda_reference ref
  | App_ref of lambda_reference ref * lambda_reference ref;;

let reduce_to_normal_form lambda = 
  let rec to_ref lambda = match lambda with 
    | Var x -> ref (Var_ref x)
    | Abs (x, l) -> ref (Abs_ref (x, to_ref l))
    | App (l1, l2) -> ref (App_ref (to_ref l1, to_ref l2)) in
  let rec to_lambda lambda = match !lambda with 
    | Var_ref x -> Var x
    | Abs_ref (x, l) -> Abs (x, to_lambda l)
    | App_ref (l1, l2) -> App (to_lambda l1, to_lambda l2) in
  let rec subst substitute expr var = match !expr with
    | Var_ref x -> if x = var then expr := !substitute
    | Abs_ref (x, l) -> if x <> var then subst substitute l var
    | App_ref (l1, l2) -> subst substitute l1 var; subst substitute l2 var in 
  let rec copy lambda map = match !lambda with 
    | Var_ref x -> 
      (try ref (Map.find x map) with _ -> ref (Var_ref x))
    | Abs_ref (x, l) -> let name = next_name() in ref (Abs_ref (name, copy l (Map.add x (Var_ref name) map)))
    | App_ref (l1, l2) -> 
      ref (App_ref (copy l1 map, copy l2 map)) in
  let rec reduction_with_memoization lambda = match !lambda with
    | Var_ref x -> false
    | Abs_ref (x, l) -> reduction_with_memoization l
    | App_ref (l1, l2) -> (match !l1 with
      | Abs_ref (x, l) -> 
        lambda := !(copy l Map.empty); subst l2 lambda x; true
      | _ -> 
        reduction_with_memoization l1 || reduction_with_memoization l2
      ) in

  let lambda_ref = copy (to_ref lambda) Map.empty in
  while reduction_with_memoization lambda_ref do () done;
  to_lambda lambda_ref;;