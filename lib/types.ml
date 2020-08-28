type component =
  | Wildcard
  | Field of string list
  | Search of string list
  | Index of int list
  | Slice of int * int option

type t = component list

exception Syntax_error of string
