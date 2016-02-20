type 'a expr =
    | Base of 'a
    | Const of bool
    | And of 'a expr list
    | Or of 'a expr list
    | Not of 'a expr;;

let rec eval expr base_eval =
    let eval' expr = eval expr base_eval in
    match expr with
    | Base base -> base_eval base
    | Const bool -> bool
    | And exprs -> List.for_all eval' exprs
    | Or exprs -> List.exists eval' exprs
    | Not expr -> not (eval' expr);;

let simplify_and l =
    if List.mem (Const false) l then Const false
    else
        match List.filter ((<>) (Const true)) l with
        | [] -> Const true
        | [x] -> x
        | l -> And l;;

let simplify_or l =
    if List.mem (Const true) l then Const true
    else
        match List.filter ((<>) (Const false)) l with
        | [] -> Const false
        | [x] -> x
        | l -> Or l;;

let simplify_not = function
    | Const b -> Const (not b)
    | e -> Not e;;

let rec simplify = function
    | Base _ | Const _ as x -> x
    | And l -> simplify_and (List.map simplify l)
    | Or l -> simplify_or (List.map simplify l)
    | Not e -> simplify_not (simplify e);;

let expr = 
    Not (
        And [
            Or [
                Base "It's snowing"; 
                Const true
            ]; 
            Base "It's raining"
        ]
    );;

let expr_simplified = simplify expr;;
assert (expr_simplified = Not (Base "It's raining"));;

