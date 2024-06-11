(* TEST

 set TSAN_OPTIONS="detect_deadlocks=0";
 modules = "waitgroup_stubs.c waitgroup.ml";

 tsan;
 native;

*)
let wg = Waitgroup.create 2

let [@inline never] writer v () =
  Waitgroup.join wg;
  Array.set v 3 0

let [@inline never] reader v =
  ignore (Sys.opaque_identity (Array.get v 3));
  Waitgroup.join wg

let () =
  let v = Array.make 4 0 in
  let d = Domain.spawn (writer v) in
  reader v;
  Domain.join d
