let fixnum_mask = 3
let fixnum_tag = 0
let fixnum_shift = 2

let char_tag = 0b00001111
let char_shift = 8

type expr =
  | Fixnum of int
  | Char of char
  | Primcall of string * expr list

let immediate_rep = function
  | Fixnum n -> n lsl fixnum_shift (* lor fixnum_tag *)
  | Char c -> (Char.code c lsl char_shift) lor char_tag
  | _ -> failwith "Unhandled immedaite rep"

let rec emit_expr f x = match x with
  | Fixnum _ | Char _  -> Printf.fprintf f "\tmovl $%d, %%eax\n" (immediate_rep x)
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
     | _ -> failwith "unknown primcall")

let compile expr =
  let f = open_out "out.s" in
  Fun.protect ~finally: (fun() -> close_out f) (fun () ->
    output_string f ".global scheme_entry\n";
    output_string f "scheme_entry:\n";
    emit_expr f expr;
    output_string f "\tret\n"
  )
