open Ast
open Stringify

let () =
  (* should match test/ex3.lol *)
  print_string (string_of_node (Program [
    Include "STDIO";
    Define "VAR";
    Loop ("LOOP", [
      Op (Inc, Var "VAR", Const_int 1);
      Output (Var "VAR");
      Break_if (Op (Gt, Var "VAR", Const_int 10))
    ])
  ]))
