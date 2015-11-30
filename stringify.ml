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

  | Assign (var, value) ->
    "LOL " ^ var ^ " R " ^ string_of_node value

  | Loop (label, body) ->
    "IM IN YR " ^ label ^ nl ^
    indent (string_of_block body) ^
    "IM OUTTA YR " ^ label

  | Break -> "ENUF"

  | If (cond, None, None) ->
    "IZ " ^ string_of_node cond ^ "? KTHX"
  | If (cond, if_body, else_body) ->
    "IZ " ^ string_of_node cond ^ "?" ^ nl ^
    begin
      maybe_body "YARLY" if_body ^
      maybe_body "NOWAI" else_body
    end ^
    "KTHX"

  | Switch (cases, default_body) ->
    let rec loop = function
      | [] -> ""
      | (value, body) :: tl ->
        "OMG " ^ string_of_node value ^ nl ^
        indent (string_of_block body) ^ loop tl
    in
    "WTF?" ^ nl ^
    indent (loop cases ^ maybe_body "OMGWTF" default_body) ^
    "OIC"

  | Input var -> "GIMMEH " ^ var
  | Output (expr, true) -> "VISIBLE " ^ string_of_node expr
  | Output (expr, false) -> "VISIBLE " ^ string_of_node expr ^ "!"

  | Open (filename, success_body, fail_body) ->
    "PLZ OPEN FILE \"" ^ filename ^ "\"?" ^ nl ^
    indent begin
      maybe_body "AWSUM THX" success_body ^
      maybe_body "O NOES" fail_body
    end

  | Increment (var, amount) -> "UP " ^ var ^ "!!" ^ string_of_int amount
  | Nop -> "WUT"

  | Unary (Neg, expr) -> "MINUZ " ^ string_of_node expr
  | Unary (Not, expr) -> "NOT " ^ string_of_node expr

  | Binary (Gt, left, right) ->
    string_of_node left ^ " BIGGER THAN " ^ string_of_node right
  | Binary (Lt, left, right) ->
    string_of_node left ^ " SMALLER THAN " ^ string_of_node right
  | Binary (Ge, left, right) ->
    string_of_node left ^ " BIGGERISH THAN " ^ string_of_node right
  | Binary (Le, left, right) ->
    string_of_node left ^ " SMALLERISH THAN " ^ string_of_node right

  | Binary (op, left, right) ->
    begin
      match op with
      | Eq  -> "BOTH SAEM"
      | Ne  -> "DIFFRINT"
      | Add -> "SUM OF"
      | Sub -> "DIFF OF"
      | Mul -> "PRODUKT OF"
      | Div -> "QUOSHUNT OF"
      | Mod -> "MOD OF"
      | And -> "BOTH OF"
      | Or  -> "EITHER OF"
      | Xor -> "WON OF"
      | Max -> "BIGGR OF"
      | Min -> "SMALLR OF"
    end ^
    " " ^ string_of_node left ^ " AN " ^ string_of_node right

  | Var var -> var
  | Const_int i -> string_of_int i
  | Const_float f -> string_of_float f
  | Const_bool true -> "WIN"
  | Const_bool false -> "FAIL"
  | Const_string s -> "\"" ^ s ^ "\""  (* TODO: escaping *)

  | Comment comment -> "BTW " ^ comment

and string_of_block b =
  let rec str buf = function
    | [] -> buf
    | hd :: tl -> str (buf ^ string_of_node hd ^ nl) tl
  in
  str "" b

and maybe_body hdr = function
  | None -> ""
  | Some body -> hdr ^ nl ^ indent (string_of_block body)
