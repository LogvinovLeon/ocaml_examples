let first_char word = word.[0];;

let last_char word = 
	let len = (String.length word) - 1 in word.[len];;

let middle word =
	let len = (String.length word) - 2 in
	String.sub word 1 len;;

let rec is_palindrome word =
	match (String.length word) with
	  (1|0) -> true
	| _ when first_char word <> last_char word -> false
	| _ -> is_palindrome (middle word);;

Printf.printf "%b\n%b\n%b\n" 
	(is_palindrome "abacaba") 
	(is_palindrome "ala ma kota") 
	(is_palindrome (String.make 100000 'a'));;