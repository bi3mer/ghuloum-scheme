open Ghuloum_scheme.Compiler

(* let () = compile (Primcall ("sub1", [Fixnum 42])) *)
(* let () = compile (Primcall ("char?", [Char 'A'])) *)
(* let () = compile (Primcall ("-", [Fixnum 5; Fixnum 3])) *)
let () = compile (Let ([("x", Fixnum 5); ("y", Fixnum 3)], Primcall ("-", [Var "x"; Var "y"])))
