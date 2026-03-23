let fixnum_mask = 3
let fixnum_tag = 0
let fixnum_shift = 2

let compile num =
  let f = open_out "out.s" in
  Fun.protect ~finally: (fun() -> close_out f) (fun () ->
    output_string f ".global scheme_entry\n";
    output_string f "scheme_entry:\n";
    Printf.fprintf f "\tmovl $%d, %%eax\n" (num lsl fixnum_shift);
    output_string f "\tret\n";
    close_out f
  )

let () = if Array.length Sys.argv < 2 then
  failwith "Usage: program <number>"
else
  compile (int_of_string Sys.argv.(1));
