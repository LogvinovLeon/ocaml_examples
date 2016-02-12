let draw_row size size_of_cell is_interline =
	begin
		for cell = 1 to size do 
		begin
			if is_interline then print_char '+'
			else print_char '|';
			for c = 1 to size_of_cell do
			begin
				if is_interline then print_char '-'
				else print_char ' ';
			end
			done
		end
		done;
		if is_interline then print_char '+'
		else print_char '|';
		print_char '\n';
	end;;

let draw_grid size size_of_cell = 
	begin
		for cell = 1 to size do
		begin
			draw_row size size_of_cell true;
			for row = 1 to size_of_cell do
				draw_row size size_of_cell false
			done
		end
		done;
		draw_row size size_of_cell true
	end;;

draw_grid 4 3;;