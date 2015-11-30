(* grammar is derived from
 * https://github.com/justinmeza/lolcode-spec/blob/master/v1.2/lolcode-spec-v1.2.md *)

type node =
  | Program of block
  | Include of string

  | Define of string * node option
  | Assign of string * node

  | Loop of string * block
  (* label, body *)
  | Break
  | If of node * block option * block option
  (* condition, if-body, else-body *)
  | Switch of (node * block) list * block option
  (* (value, body) list, default-body *)
  | Input of string
  | Output of node * bool
  (* string, output-newline *)

  | Open of string * block option * block option
  (* filename, success-body, fail-body *)
  | Increment of string * int
  | Nop

  | Unary of unary_op * node
  | Binary of binary_op * node * node
  | Var of string
  | Const_int of int
  | Const_float of float
  | Const_bool of bool
  | Const_string of string
  (* varname, filename *)

  | Comment of string

and block = node list

and iterop = UPPIN | NERFIN | Func of string

and itercond = TIL | WILE

and unary_op = Neg | Not
and binary_op =
  | Gt | Lt | Ge | Le | Eq | Ne
  | Add | Sub | Mul | Div | Mod
  | And | Or | Xor
  | Max | Min

type ty = Bool | Int | Float | String | Undef
