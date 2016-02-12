type triangle_type = 
	| Normal | Degenerated | Impossible;;

let check_triangle a b c =
	match List.sort compare [a; b; c] with
	  [a; b; c] ->
	  	begin
		  	match () with
			  () when (c < a + b)  -> Normal
			| () when (c == a + b) -> Degenerated
			| _                    -> Impossible
		end
	| _ -> failwith "WTF";;

let a = (read_int()) in
let b = (read_int()) in
let c = (read_int()) in
match (check_triangle a b c) with
  Normal      -> print_endline "Normal"
| Degenerated -> print_endline "Degenerated"
| _           -> print_endline "Impossible";;