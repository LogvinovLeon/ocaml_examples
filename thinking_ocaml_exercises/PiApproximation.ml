let rec fact_helper acc n =
	match n with
	  (0|1) -> acc
	| _     -> fact_helper (acc * n) (n - 1);;

let fact = fact_helper 1;;

let term k =
	let fk = float_of_int k in
	let numerator = ((float_of_int (fact (4 * k))) *. (1103. +. 26390. *. fk)) in
	let denumerator = (((float_of_int (fact k)) ** 4.) *. (396. ** (4. *. fk))) in
	let result = numerator /. denumerator in
	result;;

let rec compute_sum acc number_of_terms k = (
	let t = (term k) in
	match () with
	  () when k == number_of_terms - 1 -> acc +. t
	| _ -> compute_sum (acc +. t) number_of_terms (k + 1)
	);;

let approximate_pi () =
	1. /. (((2. *. (sqrt 2.)) /. 9801.) *. (compute_sum 0. 3 0));;

Printf.printf "%.17f\n" (approximate_pi ());;

