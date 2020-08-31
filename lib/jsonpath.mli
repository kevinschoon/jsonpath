type t

(*
A JSONPath can be used to search for arbitrary data within a JSON structure.
*)

val to_string : t -> string

(* convert a JSONPath into a pretty printed string *)

val of_string : string -> t

(* convert a string into a JSONPath *)

module Yojson : sig
  val select : t -> Yojson.Basic.t -> Yojson.Basic.t list
  (* Apply the components of the path
     to each JSON value in the list of values returned so far,
     starting from the root. *)
end

module Jsonm : sig
  val select : t -> Ezjsonm.value -> Ezjsonm.value list
  (* Apply the components of the path
     to each JSON value in the list of values returned so far,
     starting from the root. *)
end
