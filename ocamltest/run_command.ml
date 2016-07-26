(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sébastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Run programs and log their stdout/stderr, with a timer... *)

type settings = {
  progname : string;
  argv : string array;
  (* envp : string array; *)
  stdin_filename : string;
  stdout_filename : string;
  stderr_filename : string;
  append : bool;
  timeout : int;
  log : out_channel;
}

let settings_of_commandline commandline =
  let words = Testlib.words commandline in
  {
    progname = List.hd words;
    argv = Array.of_list words;
    stdin_filename = "";
    stdout_filename = "";
    stderr_filename = "";
    append = false;
    timeout = 0;
    log = stderr
  }

external run : settings -> int = "caml_run_command"

let run_commandline commandline = run (settings_of_commandline commandline)
