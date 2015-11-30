open Ast

let nl = "\n"

let tab = "    "

let indent = Str.global_replace (Str.regexp "^\\(.\\)") (tab ^ "\\1")

let rec cat sep fn = function
  | [] -> ""
  | [hd] -> fn hd
  | hd :: tl -> fn hd ^ sep ^ cat sep fn tl

let rec prefix_all prefix = function
  | [] -> ""
  | hd :: tl -> prefix ^ hd ^ prefix_all prefix tl

let rec string_of_node = function
  | Program block ->
    "HAI" ^ nl ^
    string_of_block block ^
    "KTHXBYE"

  | Include libname -> "CAN HAS " ^ libname ^ "?"

  | Define (var, None) ->
    "I HAS A " ^ var
  | Define (var, Some value) ->
    "I HAS A " ^ var ^ " ITZ " ^ string_of_node value

  | Loop (label, body) ->
    "IM IN YR " ^ label ^
      indent (string_of_block body) ^
    "IM OUTTA YR " ^ label

  | If (cond, if_block, else_block) ->
    "O RLY? " ^ ...

  | Switch ()

  | Output expr -> "VISIBLE " ^ string_of_node expr

  | Break -> "ENUF"
  | Break_if cond -> "IZ " ^ string_of_node cond ^ "? KTHX"

  | Const_int i -> string_of_int i
  | Const_float f -> string_of_float f
  | Const_bool true -> "WIN"
  | Const_bool false -> "FAIL"
  | Const_string s -> "\"" ^ s ^ "\""  (* TODO: escaping *)

  (*TODO | Op (op, args)*)

  | Comment (node, comment) -> string_of_node node ^ "  BTW " ^ comment
  | Multiline_comment comment -> "OBTW" ^ nl ^ indent comment ^ nl ^ "TLDR"

and string_of_block b =
  let rec str buf = function
    | [] -> buf
    | hd :: tl -> str (buf ^ string_of_node hd ^ nl) tl
  in
  str "" b
