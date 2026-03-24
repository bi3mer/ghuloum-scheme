let fixnum_tag   = 0b00        (* 0 *)
let fixnum_shift = 0b00000010  (* 2 *)
let fixnum_mask  = 0b11        (* 3 *)

let char_tag   = 0b00001111  (*  15 *)
let char_shift = 0b00001000  (*   8 *)
let char_mask  = 0b11111111  (* 255 *)

let bool_tag   = 0b00101111   (*  47 *)
let bool_shift = 0b00000111   (*   7 *)
let bool_mask  = 0b01111111   (* 127 *)

type expr =
  | Bool of bool
  | Char of char
  | Fixnum of int
  | Primcall of string * expr list

let immediate_rep = function
  | Bool b -> ((if b then 1 else 0) lsl bool_shift) lor bool_tag
  | Char c -> (Char.code c lsl char_shift) lor char_tag
  | Fixnum n -> n lsl fixnum_shift (* lor fixnum_tag *)
  | _ -> failwith "Unhandled immedaite rep"

let emit_zeroflag_to_bool f =
  Printf.fprintf f "\tmovl $0, %%eax\n";             (* clear eax so upper bits don't corrupt result *)
  Printf.fprintf f "\tsete %%al\n";                  (* set low byte to 1 if zero flag set, else 0 *)
  Printf.fprintf f "\tsall $%d, %%eax\n" bool_shift; (* shift 0/1 into bool payload position *)
  Printf.fprintf f "\torl $%d, %%eax\n" bool_tag     (* OR in bool tag bits *)

let emit_type_predicate f mask tag =
  Printf.fprintf f "\tandl $%d, %%eax\n" mask;  (* mask to isolate tag bits *)
  Printf.fprintf f "\tcmpl $%d, %%eax\n" tag;   (* compare against expected tag, sets zero flag *)
  emit_zeroflag_to_bool f

(* TODO
  [ ] null?
  [ ] zero?
  [ ] not
*)

let rec emit_expr f x = match x with
  | Fixnum _ | Char _ | Bool _  -> Printf.fprintf f "\tmovl $%d, %%eax\n" (immediate_rep x)
  | Primcall (op, args) ->
    (match op with
     | "add1" ->
       emit_expr f (List.hd args);
       Printf.fprintf f "\taddl $%d, %%eax\n" (immediate_rep (Fixnum 1))

     | "sub1" ->
       emit_expr f (List.hd args);
       Printf.fprintf f "\tsubl $%d, %%eax\n" (immediate_rep (Fixnum 1))

     | "integer->char" ->
        emit_expr f (List.hd args);
        Printf.fprintf f "\tshll $%d, %%eax\n" (char_shift - fixnum_shift);
        Printf.fprintf f "\torl $%d, %%eax\n" char_tag

     | "char->integer" ->
        emit_expr f (List.hd args);
        Printf.fprintf f "\tshrl $%d, %%eax\n" (char_shift - fixnum_shift);

     | "bool?" ->
       emit_expr f (List.hd args);
       emit_type_predicate f bool_mask bool_tag

     | "char?" ->
       emit_expr f (List.hd args);
       emit_type_predicate f char_mask char_tag

     | "integer?" ->
       emit_expr f (List.hd args);
       emit_type_predicate f fixnum_mask fixnum_tag

     | _ -> failwith "unknown primcall")

let compile expr =
  let f = open_out "out.s" in
  Fun.protect ~finally: (fun() -> close_out f) (fun () ->
    output_string f ".global scheme_entry\n";
    output_string f "scheme_entry:\n";
    emit_expr f expr;
    output_string f "\tret\n"
  )
