type polynomial = float list
(* List can be empty *)
(* Trailing zeros ale not allowed *)

val eval : polynomial -> float -> float

val sum : polynomial -> polynomial -> polynomial

val mult : polynomial -> polynomial -> polynomial

val derivative : polynomial -> polynomial

val integral : polynomial -> polynomial

val degree : polynomial -> int

val ones : int -> polynomial

val zeros_ones : int -> polynomial
