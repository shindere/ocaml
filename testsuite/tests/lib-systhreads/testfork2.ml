(* TEST
   * hassysthreads
   include systhreads
   ** not-bsd
   *** libunix
   **** bytecode
   **** native
*)

(* POSIX threads and fork() *)

let alloc_string () = ignore(String.make 2048 '0')

let compute_thread () =
  Thread.create begin fun () ->
    alloc_string ()
  end ()

let fork () =
  match Unix.fork() with
  | 0 -> alloc_string (); print_endline "passed"
  | pid -> exit 0

let main () =
  ignore(compute_thread ());
  ignore(compute_thread ());
  ignore(compute_thread ());
  fork ()

let _ = main()
