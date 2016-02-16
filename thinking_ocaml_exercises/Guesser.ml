let rec guess l r =
	if r - l == 1 
	then Printf.printf "The number is: %d\n" l
	else 
		let m = (l + r) / 2 in begin
			Printf.printf "Is the number bigger or equal than: %d  (yes/no): " m;
			let answer = read_line() in
			match answer with 
			  "yes" -> guess m r
			| "no"  -> guess l m
			| _     -> failwith "Invalid user input"
		end;;

let upper_bound = 100 in begin
Printf.printf "Please think about the number (between %d & %d) and  will guess it" 1 upper_bound;
print_endline "Press ENTER when you're done...";
ignore (read_line());
guess 1 (upper_bound + 1)
end;;