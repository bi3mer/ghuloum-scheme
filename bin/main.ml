let compile num =
  let f = open_out "out.s" in
  output_string f ".global scheme_entry\n";
  output_string f "scheme_entry:\n";
  Printf.fprintf f "\tmovl $%d, %%eax\n" num;
  output_string f "\tret\n";
  close_out f

let () = if Array.length Sys.argv < 2 then
  failwith "Usage: program <number>"
else
  compile (int_of_string Sys.argv.(1));
