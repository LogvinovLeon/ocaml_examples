let string_repeat s n =
  Array.fold_left (^) "" (Array.make n s);;

let char_inside is_interline = 
	match is_interline with
	  true -> '-'
	| _	   -> ' ';;

let char_outside is_interline = 
	match is_interline with
	  true -> '+'
	| _	   -> '|';;

let draw_row size size_of_cell is_interline =
	let outside = String.make 1 (char_outside is_interline) in
	let inside = char_inside is_interline in
	(string_repeat (outside ^ (String.make size_of_cell inside)) size) ^ outside ^ "\n";;

let draw_grid size size_of_cell = 
	let outside = (draw_row size size_of_cell true) in
	let inside = (draw_row size size_of_cell false) in
	(string_repeat (outside ^ (string_repeat inside size_of_cell)) size) ^ outside;;

let grid = draw_grid 4 3 in
	print_string grid;;