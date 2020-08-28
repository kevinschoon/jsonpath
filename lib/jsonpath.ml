open Core_kernel

type t = Types.component list

(* Depth-first search through a JSON value
   for all values associated with any key in names. *)
let search names json =
  let rec collect name = function
    | `Bool _ | `Float _ | `Int _ | `Null | `String _ -> []
    | `Assoc obj ->
        List.concat_map obj ~f:(fun (key, value) ->
            let found = collect name value in
            if String.equal key name then value :: found else found)
    | `List l -> List.concat_map l ~f:(collect name)
  in
  List.fold names ~init:[] ~f:(fun results name ->
      List.append results (collect name json))

let all_sub_values = function
  | `Bool _ | `Float _ | `Int _ | `Null | `String _ -> []
  | `Assoc obj -> List.map obj ~f:snd
  | `List l -> l

(* Perform a path operation on a single JSON value.
   Each operation may return multiple JSON results. *)
let eval_component operation json =
  let module J = Yojson.Basic.Util in
  match operation with
  | Types.Wildcard -> all_sub_values json
  | Types.Field names -> List.map names ~f:(fun name -> J.member name json)
  | Types.Search names -> search names json (*search names json*)
  | Types.Index idxs ->
      let a = Array.of_list (J.to_list json) in
      List.map idxs ~f:(fun i -> a.(i))
  | Types.Slice (start, maybe_stop) ->
      let l = J.to_list json in
      let max_stop = List.length l in
      let stop = Option.value maybe_stop ~default:max_stop in
      let clip i = if i < 0 then 0 else if i > max_stop then max_stop else i in
      List.slice l (clip start) (clip stop)

let select path json =
  let apply jsons oper = List.concat_map jsons ~f:(eval_component oper) in
  List.fold path ~init:[ json ] ~f:apply

let print_component =
  let comma = String.concat ~sep:"','" in
  let json_string s = Yojson.Basic.to_string (`String s) in
  function
  | Types.Wildcard -> "[*]"
  | Types.Field names -> "['" ^ comma (List.map names ~f:json_string) ^ "']"
  | Types.Search names -> "..['" ^ comma (List.map names ~f:json_string) ^ "']"
  | Types.Index idxs -> "[" ^ comma (List.map idxs ~f:string_of_int) ^ "]"
  | Types.Slice (start, None) -> "[" ^ string_of_int start ^ ":]"
  | Types.Slice (start, Some stop) ->
      "[" ^ string_of_int start ^ ":" ^ string_of_int stop ^ "]"

let of_string path =
  let buf = Lexing.from_string path in
  let path = Parser.path Lexer.token buf in
  path

let to_string path = "$" ^ String.concat (List.map path ~f:print_component)
