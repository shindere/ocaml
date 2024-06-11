(* TEST

 ocamlopt_flags = "-g";

 modules = "waitgroup_stubs.c waitgroup.ml";

 set TSAN_OPTIONS="detect_deadlocks=0";

 tsan;
 frame_pointers;
 native;
*)
exception ExnA
exception ExnB

open Printf

let wg = Waitgroup.create 2
let r = ref 0

let [@inline never] race () =
  ignore @@ !r;
  Waitgroup.join wg

let [@inline never] i () =
  printf "Entering i\n%!";
  printf "Throwing ExnA...\n%!";
  ignore (raise ExnA);
  printf "Leaving i\n%!"

let [@inline never] h () =
  printf "Entering h\n%!";
  try i () with
  | ExnB -> printf "Caught an ExnB\n%!";
  printf "Leaving h\n%!"

let [@inline never] g () =
  printf "Entering g\n%!";
  h ();
  printf "Leaving g\n%!"

let [@inline never] f () =
  printf "Entering f\n%!";
  (try g () with
  | ExnA ->
    printf "Caught an ExnA\n%!";
    Printexc.print_backtrace stdout;
    race ());
  printf "Leaving f\n%!"

let [@inline never] writer () =
  Waitgroup.join wg;
  r := 1

let () =
  Printexc.record_backtrace true;
  let d = Domain.spawn writer in
  f ();
  Domain.join d
