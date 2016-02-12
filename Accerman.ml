let rec accerman m n : int =
	match () with
	  () when m == 0 -> n + 1
	| () when (n == 0 && m > 0) -> accerman (m - 1) 1
	| () when (n > 0 && m > 0) -> accerman (m - 1) (accerman m (n - 1))
	| () -> failwith "WTF";;

Printf.printf "%d\n" (accerman 4 5);; 