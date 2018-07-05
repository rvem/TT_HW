type algebraic_term 
  = Var of string 
  | Fun of string * (algebraic_term list)

let system_to_equation s = 
  let rec gen_name term = match term with
    | Var s -> s
    | Fun (f, l) -> f ^ (List.fold_left (^) "" (List.map gen_name l)) in
  let name = 
    "F" ^ (List.fold_left (^) "" (List.map (fun (t1, t2) -> gen_name t1 ^ gen_name t2) s)) in
  let (x, y) = List.split s in
  (Fun (name, x), Fun (name, y))
;;

let rec apply_substitution s var = match var with
  | Var x -> (try let (f, y) = 
      List.find (fun (t, e) -> t = x) s in y with Not_found -> var)
  | Fun (f, l) -> Fun (f, List.map (fun x -> apply_substitution s x) l)
;;

let check_solution x1 x2 =
  let rec check t1 t2 = match (t1, t2) with
  | (Var x, Var y) -> x = y
  | (Fun (f1, l1), Fun (f2, l2)) -> f1 = f2 && List.for_all2 check l1 l2
  | _ -> false in
  List.for_all 
    (fun (t1, t2) -> check (apply_substitution x1 t1) (apply_substitution x1 t2)) x2
;;

exception Unify_exception;;

let solve_system s =
  let rec check t1 t2 = match (t1, t2) with
    | (Var x, Var y) -> x = y
    | (Fun (f1, l1), Fun (f2, l2)) -> f1 = f2 && List.for_all2 check l1 l2
    | _ -> false in
  let rec is_used v t = match t with
    | Var x -> x = v
    | Fun (f, l) -> List.exists (is_used v) l in
  let subst v t = fun (x, y) -> (apply_substitution [(v, t)] x, apply_substitution [(v, t)] y) in
  let rec unify_system l1 l2 = match l1 with
    | (t1, t2) :: tail -> 
      (if check t1 t2 then
        unify_system tail l2
      else
        (match (t1, t2) with
        | (Fun (f1, l1'), Fun (f2, l2')) -> 
            if (f1 = f2) then
              unify_system (List.append (List.combine l1' l2') tail) l2
            else
              raise Unify_exception
        | (Fun (f, l), Var x) -> unify_system ((t1, t2) :: tail) l2
        | (Var x, _) -> 
          if is_used x t2 then
            raise Unify_exception
          else
            unify_system (List.map (subst x t2) l1) ((t1, t2) :: (List.map (subst x t2) l2))
        )
      )
    | [] -> List.map (
        fun (t1, t2) -> match (t1, t2) with 
          | (Var x, _) -> (x, t2) 
          | _ -> raise Unify_exception
        ) l2 in
  try 
    Some (unify_system s []) 
  with 
    Unify_exception -> None
;;
