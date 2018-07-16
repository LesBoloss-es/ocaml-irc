
let to_string_of_pp_print pp_print x =
  let buf = Buffer.create 8 in
  let fmt = Format.formatter_of_buffer buf in
  pp_print fmt x;
  Format.pp_print_flush fmt ();
  Buffer.contents buf
