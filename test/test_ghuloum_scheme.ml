open Ghuloum_scheme.Compiler

let run expr =
  compile expr;
  ignore (Sys.command "gcc ../../../startup.c out.s -o out");
  let ic = Unix.open_process_in "./out" in
  let result = input_line ic in
  ignore (Unix.close_process_in ic);
  result

let run_int expr = int_of_string (run expr)
let run_char expr = (run expr).[0]

(* *)
let test_fixnum () =
  Alcotest.(check int) "42 encodes correctly" (42 lsl fixnum_shift) (immediate_rep (Fixnum 42));
  Alcotest.(check int) "0 encodes correctly" (0 lsl fixnum_shift) (immediate_rep (Fixnum 0));
  Alcotest.(check int) "-1 encodes correctly" ((-1) lsl fixnum_shift) (immediate_rep (Fixnum (-1)))

(* *)
let test_char () =
  Alcotest.(check int) "char a" ((Char.code 'a' lsl char_shift) lor char_tag) (immediate_rep (Char 'a'));
  Alcotest.(check int) "char A" ((Char.code 'A' lsl char_shift) lor char_tag) (immediate_rep (Char 'A'));
  Alcotest.(check int) "char 0" ((Char.code '0' lsl char_shift) lor char_tag) (immediate_rep (Char '0'))

(* *)
let test_unknown_primcall () =
  Alcotest.check_raises "unknown primcall fails"
    (Failure "unknown primcall")
    (fun () -> compile (Primcall ("asdf", [Fixnum 1])));
  Alcotest.check_raises "another unknown primcall fails"
      (Failure "unknown primcall")
      (fun () -> compile (Primcall ("foo", [Fixnum 1])));
  Alcotest.check_raises "another unknown primcall fails"
     (Failure "unknown primcall")
     (fun () -> compile (Primcall ("", [Fixnum 1])))

(* *)
let test_add1 () =
  Alcotest.(check int) "add1 -1 -> 0" 0 (run_int (Primcall ("add1", [Fixnum (-1)])));
  Alcotest.(check int) "add1 6 -> 7" 7 (run_int (Primcall ("add1", [Fixnum 6])))

(* *)
let test_sub1 () =
  Alcotest.(check int) "sub1 0 -> -1" (-1) (run_int (Primcall ("sub1", [Fixnum 0])));
  Alcotest.(check int) "sub1 8 -> 7" 7 (run_int (Primcall ("sub1", [Fixnum 8])))

(* *)
let test_integer_to_char () =
  Alcotest.(check char) "int 97 -> char a" 'a' (run_char (Primcall ("integer->char", [Fixnum 97])));
  Alcotest.(check char) "int 65 -> char A" 'A' (run_char (Primcall ("integer->char", [Fixnum 65])));
  Alcotest.(check char) "int 48 -> char 0" '0' (run_char (Primcall ("integer->char", [Fixnum 48])))

(* *)
let test_char_to_integer () =
  Alcotest.(check int) "char a -> 97" 97 (run_int (Primcall ("char->integer", [Char 'a'])));
  Alcotest.(check int) "char A -> 65" 65 (run_int (Primcall ("char->integer", [Char 'A'])));
  Alcotest.(check int) "char 0 -> 48" 48 (run_int (Primcall ("char->integer", [Char '0'])))

(* *)
let test_char_roundtrip () =
  Alcotest.(check char) "int->char->int a" 'a'
    (run_char (Primcall ("integer->char", [Primcall ("char->integer", [Char 'a'])])));
  Alcotest.(check int) "char->int->char 97" 97
    (run_int (Primcall ("char->integer", [Primcall ("integer->char", [Fixnum 97])])))

let () =
  Alcotest.run "ghuloum" [
    "immediate_rep", [
      Alcotest.test_case "fixnum" `Quick test_fixnum;
      Alcotest.test_case "fixchar" `Quick test_char;
      Alcotest.test_case "nonsense primcall" `Quick test_unknown_primcall;
      Alcotest.test_case "add1" `Quick test_add1;
      Alcotest.test_case "sub1" `Quick test_sub1;
      Alcotest.test_case "integer->char" `Quick test_integer_to_char;
      Alcotest.test_case "char->integer" `Quick test_char_to_integer;
      Alcotest.test_case "test_char_round_trip" `Quick test_char_roundtrip;
    ]
  ]
