type peano = Z 
			| S of peano;; (* òèïû íåîáõîäèìî êîïèðîâàòü â ðåàëèçàöèþ *)
type lambda = Var of string 
			| Abs of string * lambda 
			| App of lambda * lambda;;

let rec peano_of_int x = match x with
 	| 0 -> Z
 	| _ -> S (peano_of_int (x - 1));;

let rec int_of_peano p = match p with
    | Z -> 0
  	| S x -> 1 + int_of_peano x;;

let inc x = S x;;
let rec add x y = match y with
	| Z -> x
	| S yy -> S(add x yy);;
let rec sub x y = match (x, y) with
	| (Z, yy) -> Z
	| (xx, Z) -> x
	| (S xx, S yy) -> sub xx yy;;
let rec mul x y = match (x, y) with
	| (xx, Z) -> Z
	| (Z, yy) -> Z
	| (xx, S yy) -> add (mul xx yy) (x);;
let rec power x y = match (x, y) with
	| (Z, xx) -> Z
	| (xx, Z) -> S Z
	| (S Z, xx) -> S Z
	| (xx, S yy) -> mul (power xx yy) (xx);; 
                     
let rev x = failwith "Not implemented";;
let merge_sort x = failwith "Not implemented";;
                     
let string_of_lambda x = failwith "Not implemented";;
let lambda_of_string x = failwith "Not implemented";;