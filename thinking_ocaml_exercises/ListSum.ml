let list_sum l =
	let sum = (+) in
	let acc = 0 in
	List.fold_left sum acc l;;