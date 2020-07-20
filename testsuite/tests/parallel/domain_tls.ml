(* TEST
* hasunix
include unix
** bytecode
** native
*)

let check () =
  let k1 : int Domain.TLS.key = Domain.TLS.new_key () in
  let k2 : float Domain.TLS.key = Domain.TLS.new_key () in
  Domain.TLS.set k1 100;
  Domain.TLS.set k2 200.0;
  let v1 = Option.get (Domain.TLS.get k1) in
  let v2 = Option.get (Domain.TLS.get k2) in
  v1 + (int_of_float v2) |> ignore

let _ =
  let domains = Array.init 3 (fun _ -> Domain.spawn(check)) in
  check ();
  Array.iter Domain.join domains;
  print_endline "ok"
