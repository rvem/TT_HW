type peano = Z | S of peano

val peano_of_int: int -> peano
val int_of_peano: peano -> int

val inc: peano -> peano
val add: peano -> peano -> peano
val sub: peano -> peano -> peano
val mul: peano -> peano -> peano
val power: peano -> peano -> peano

val rev: 'a list -> 'a list
val merge_sort: 'a list -> 'a list

open Lambda;;

val string_of_lambda: lambda -> string

(*
   <lambda> -> <expr> ' ' <abs> | <expr> | <abs>
   <abs> -> \<var>.<lambda>
   <expr> -> { <var> | (<lambda>) }+{' '}
*)

val lambda_of_string: string -> lambda
