open Core.Std
open Async.Std
open Cohttp_async

let query_uri query =
    let base_uri = Uri.of_string "http://api.duckduckgo.com/?format=json" in
    Uri.add_query_param base_uri ("q" , [query])

let get_definition_from_json json =
    match Yojson.Safe.from_string json with
    | `Assoc kv_list ->
        let find key =
            begin match List.Assoc.find kv_list key with
                | None | Some (`String "") -> None
                | Some s -> Some (Yojson.Safe.to_string s)
            end
        in
        begin match find "Abstract" with
        | Some _ as x -> x
        | None -> find "Definition"
        end
    | _ -> None

let get_definition word =
    Client.get (query_uri word)
    >>= fun (_, body) ->
    Body.to_string body
    >>| fun (body) ->
    (word, get_definition_from_json body)


let print_result (word,definition) =
  printf "%s\n%s\n\n%s\n\n"
    word
    (String.init (String.length word) ~f:(fun _ -> '-'))
    (match definition with
    | None -> "No definition found"
    | Some def ->
      String.concat ~sep:"\n"
        (Wrapper.wrap (Wrapper.make 70) def))

let search_and_print words =
    Deferred.all (List.map words ~f:get_definition)
    >>| fun results ->
        List.iter results ~f:print_result;;

let () =
    Command.async_basic
        ~summary:"Retrieve definitions from duckduckgo search engine"
        Command.Spec.(
            empty
            +> anon (sequence ("word" %: string))
        )
        (fun words () -> search_and_print words)
    |> Command.run
