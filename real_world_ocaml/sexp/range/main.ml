open Core.Std

let intervals =
  let module I = Range in
  [ I.create 3 4;
    I.create 5 4; (* should be empty *)
    I.create 2 3;
    I.create 1 6;
  ]

let () =
  intervals
  |> List.sexp_of_t Range.sexp_of_t
  |> Sexp.to_string_hum
  |> print_endline
