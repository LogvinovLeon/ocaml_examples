type polynomial = float list

(* Helper functions *)

let list_of l n =
    let rec _list_of acc l n  =
        if n == 0 then acc
        else _list_of (l@acc) l (n - 1) in
    _list_of [] l n;;


let zeros = list_of [0.];;
let ones = list_of [1.];;
let zeros_ones = list_of [0.; 1.];;

let justify l1 l2 =
    let len1 = List.length l1 in
    let len2 = List.length l2 in
    let len = max len1 len2 in
    (l1@(zeros (len - len1)), l2@(zeros (len - len2)));;

let rec normalize = function
      [] -> []
    | hd::tl -> match normalize tl with
                  [] when hd = 0. ->[]
                | normalized -> hd::normalized;; 

let shift_right p n =
    normalize ((zeros n)@p);;

let mult_by_float p f =
    List.map (fun a -> a*.f) p;;

(* Helper functions end *)

let eval p x =
    List.fold_right (fun a v -> v *. x +. a) p 0.;;

let sum p1 p2 =
    let (p1, p2) = justify p1 p2 in
    normalize (List.map2 (+.) p1 p2);;

let mult p1 p2 =
    let terms = List.mapi (fun i a -> (shift_right (mult_by_float p1 a) i)) p2 in
    List.fold_right sum terms [];;

let derivative p =
    normalize (List.tl (List.mapi (fun i a -> a *. (float_of_int i)) p));;

let integral p =
    normalize (0.::(List.mapi (fun i a -> a /. (float_of_int (i + 1))) p));;

let degree p =
    (List.length p) - 1;;

