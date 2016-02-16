let right_justify width s = 
	let prefix_len = width - (String.length s) in
	let prefix = String.make  prefix_len ' ' in
	prefix ^ s;;

let right_justify_80 = right_justify 80;;

print_endline (right_justify_80 "dupa");;
