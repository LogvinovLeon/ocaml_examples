let rec fib_helper f_prev f_prev_prev i n =
	match n with
	  0 -> 0
	| 1 -> 1
	| j when j == i -> f_prev + f_prev_prev
	| _ -> fib_helper f_prev_prev (f_prev + f_prev_prev) (i + 1) n;;

let fib_tail = fib_helper 0 1 2;;

let rec fib_naive = function
	  0 -> 0
	| 1 -> 1
	| n -> (fib_naive (n - 1)) + (fib_naive (n - 2));;


let time f x =
    let t = Sys.time() in
    let fx = f x in (
    	Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    	fx
	);;	

Printf.printf "%d\n" (time fib_tail 10000000);;
(* Execution time: 0.450613s *)
Printf.printf "%d\n" (time fib_naive 40);;
(* Execution time: 11.699451s *)