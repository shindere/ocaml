(* TEST
  include testing
  ocamlopt_flags = "-inline 20"
*)

let f x = Sys.opaque_identity x

let () =
  assert(f f == f);
  assert(Sys.opaque_identity 1 = 1);
  assert(Sys.opaque_identity 1. = 1.)
