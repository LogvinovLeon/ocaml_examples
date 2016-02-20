type color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White;;

let int_of_color = function
    | Black -> 0 | Red     -> 1 | Green -> 2 | Yellow -> 3 
    | Blue  -> 4 | Magenta -> 5 | Cyan  -> 6 | White  -> 7;;

let csprintf color =
    Printf.ksprintf (Printf.sprintf "\027[38;5;%dm%s\027[0m" (int_of_color color));;

let rprinter = csprintf Red;;
let bprinter = csprintf Blue;;
let gprinter = csprintf Green;;
let wprinter = csprintf White;;

print_endline (rprinter "%s" "Roses are red");;
print_endline (bprinter "%s" "Violets are blue");;
print_endline (gprinter "%s" "My terminal prompt is green");;
print_endline (wprinter "%s" "Ocaml I love you");;

