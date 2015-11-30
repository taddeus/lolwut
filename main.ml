open Ast
open Stringify

let () =
  (* should match test/ex1.lol *)
  print_string (string_of_node (Program [
    Include "STDIO";
    Output (Const_string "HAI WORLD!", true)
  ]));

  print_endline "\n";

  (* should match test/ex2.lol *)
  print_string (string_of_node (Program [
    Include "STDIO";
    Open ("LOLCATS.TXT",
      Some [Output (Var "FILE", true)],
      Some [Output (Const_string "ERROR!", true)])
  ]));

  print_endline "\n";

  (* should match test/ex3.lol *)
  print_string (string_of_node (Program [
    Include "STDIO";
    Define ("VAR", None);
    Loop ("LOOP", None, None, [
      Increment ("VAR", 1);
      Output (Var "VAR", true);
      If (Binary (Gt, Var "VAR", Const_int 10), None, None)
    ])
  ]));

  print_endline "\n";

  (* should match test/ex4.lol *)
  print_string (string_of_node (Program [
    Include "STDIO";
    Loop (
      "LOOP",
      Some (Inc "VAR"),
      Some (Until (Binary (Eq, Var "VAR", Const_int 10))),
      [Output (Binary (Add, Var "VAR", Const_int 1), true)]
    )
  ]))
