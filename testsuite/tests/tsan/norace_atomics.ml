(* TEST

 set TSAN_OPTIONS="detect_deadlocks=0";

 modules = "waitgroup_stubs.c waitgroup.ml";

 tsan;
 native;

*)

let wg = Waitgroup.create 2
let v = Atomic.make 0

let [@inline never] writer () =
  Waitgroup.join wg;
  Atomic.set v 10

let [@inline never] reader () =
  ignore (Sys.opaque_identity (Atomic.get v));
  Waitgroup.join wg

let () =
  let d = Domain.spawn writer in
  reader ();
  Domain.join d
