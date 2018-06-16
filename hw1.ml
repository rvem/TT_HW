type peano = Z | S of peano;;
open Lambda;;


let rec make_peano l r =
	match l with
		| 0 -> r
		| x -> S(make_peano (x - 1) r);;

let peano_of_int x = make_peano x Z;;


let rec int_of_peano p = match p with
 	| Z -> 0
 	| S x -> 1 + int_of_peano x;;

let rec add x y = match y with
	| Z-> x
	| S yy -> S(add x yy);;

let inc x = add x (S Z);;

let rec sub x y = match (x, y) with 
	| (xx, Z)-> xx
	| (Z, yy) -> Z
	| (S xx, S yy)-> sub xx yy;;

let rec mul x y = match (x, y)  with
	| (xx, Z) -> Z
	| (Z, yy) -> Z
	| (xx, S yy) -> add (mul xx yy) (xx);;

let rec power x y = match (x, y) with
	| (xx, Z) -> S Z
	| (Z, xx) -> Z
	| (S Z, xx) -> S Z
	| (xx, S yy) -> mul (power xx yy) xx;;
                     
let rec tail_add list1 list2 = match list1 with
	| [] -> list2
	| h::t -> tail_add t (h::list2)

let rev x = tail_add x [];;

let rec split x y z = match x with
	| [] -> (y, z)
	| x::tail -> split (tail) (z) (x::y);;

let rec merge x y = match (x, y) with
	| ([], _) -> y
	| (_, []) -> x
	| (head1::tail1, head2::tail2) ->
		if ((<) head1 head2) then 
			(head1::(merge tail1 y))
		else
			(head2::(merge x tail2));;

let rec merge_sort x = match x with
	| [] -> x
	| (_::[]) -> x
	| _ -> let (left, right) = split x [] [] in
	(merge (merge_sort left) (merge_sort right));;
                     
let string_of_lambda x = 
	let rec to_string lambda str = 
		match lambda with
		| Var(x) -> str ^ x
		| Abs(x, y) -> str ^ "(\\" ^ x ^ "." ^ (to_string y "") ^ ")"
		| App(x, y) -> str ^ "(" ^ (to_string x "") ^ " " ^ (to_string y "") ^ ")" in
	to_string x "";;

let lambda_of_string x = Parser.parse Lexer.token (Lexing.from_string (String.trim x));;
