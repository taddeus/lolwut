(* grammar is derived from
 * https://github.com/justinmeza/lolcode-spec/blob/master/v1.2/lolcode-spec-v1.2.md *)

type node =
  (* top-level *)

  | Program of block
  | Include of string
  | Define of string * node option

  (* statements *)

  | Loop of string * (iterop * string) option * (itercond * node) option * block
  (* label, [operation], [condition], body *)
  | If of node * block * block
  (* condition, if-body, else-body *)
  | Switch of (node * block) list
  (* (const value, body) list *)
  | Output of node
  | Break
  | Break_if of node

  (* expressions *)

  | Op of op * node list
  | Const_int of int
  | Const_float of float
  | Const_bool of bool
  | Const_string of string

  (* comments *)

  | Comment of node * string
  | Multiline_comment of string

and block = node list

and iterop = UPPIN | NERFIN | Func of string

and itercond = TIL | WILE

and op =
  | Add | Sub | Mul | Div | Mod | Max | Min
  | And | Or | Xor

type ty = Bool | Int | Float | String | Undef
