include Format

let to_string_of_pp_print pp_print_v v =
  let buf = Buffer.create 8 in
  let fmt = Format.formatter_of_buffer buf in
  pp_print_v fmt v;
  Format.pp_print_flush fmt ();
  Buffer.contents buf
