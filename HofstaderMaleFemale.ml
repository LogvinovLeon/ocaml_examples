let rec female = function
	  0 -> 1
	| n -> (n - male (female (n - 1)))
and male = function
	  0 -> 0
	| n -> (n - female (male (n - 1)));;

let list = [0; 1; 2; 3; 4; 5; 6; 7; 8] in
List.rev_map2 (fun a b -> Printf.printf "%d %d\n" (male a) (female b)) list list;;