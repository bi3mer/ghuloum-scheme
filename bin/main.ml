open Ghuloum_scheme.Compiler

(* let () = compile (Primcall ("sub1", [Fixnum 42])) *)
let () = compile (Primcall ("char?", [Char 'A']))
