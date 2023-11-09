(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Processing of ocamltest's command-line options *)

open Ocamltest_stdlib
open Operations

let show_objects title string_of_object objects =
  let print_object o = print_endline ("  " ^ (string_of_object o)) in
  print_endline title;
  List.iter print_object objects;
  exit 0

let string_of_action a =
  Printf.sprintf "%s: %s" (Actions.name a) (Actions.description a)

let string_of_test test =
  if test.Tests.test_run_by_default then
    test.Tests.test_name ^ " (run by default): " ^ test.Tests.test_description
  else
    test.Tests.test_name ^ ": " ^ test.Tests.test_description

let string_of_variable v =
  Printf.sprintf "%s: %s"
    (Variables.name_of_variable v)
    (Variables.description_of_variable v)

let show_actions () =
  let actions = Actions.get_registered_actions () in
  show_objects "Available actions are:" string_of_action actions

let show_tests () =
  let tests = Tests.get_registered_tests () in
  show_objects "Available tests are:" string_of_test tests

let show_variables () =
  let variables = Variables.get_registered_variables () in
  show_objects "Available variables are:" string_of_variable variables

let op = ref (Run_tests default_test_settings)

let error msg =
  failwith ("cannot " ^ msg ^ " in " ^ (to_string !op) ^ " mode")
  
let set_log_to_stderr () =
  match !op with
  | Run_tests r ->
    op  := Run_tests {r with log_to_stderr = true }
  | _ -> error "log to stderr"

let set_promote () =
  match !op with
  | Run_tests r ->
    op  := Run_tests {r with promote = true }
  | _ -> error "promote"

let set_show_timings () =
  match !op with
  | Run_tests r ->
    op  := Run_tests {r with show_timings = true }
  | _ -> error "show timings"

let set_keep_test_dir_on_success () =
  match !op with
  | Run_tests r ->
    op := Run_tests { r with keep_test_dir_on_success = true }
  | _ -> error "keep test dirs on success"

let set_timeout t =
  match !op with
  | Run_tests r ->
    if t > 0
    then op := Run_tests {r with default_timeout = t }
    else raise (Arg.Bad "negative timeout")
  | _ -> error "timeout"

let set_find_test_dirs dir =
  let dirs =
  match !op with
    | Find_test_dirs l -> l
    | _ -> []
  in
  op := Find_test_dirs (dirs @ [dir])

let set_list_tests dir =
  let dirs =
  match !op with
    | List_tests l -> l
    | _ -> []
  in
  op := List_tests (dirs @ [dir])

let set_translate () =
  op := Translate_tests default_translate_settings

let set_compact () =
  match !op with
  | Translate_tests r ->
    op  := Translate_tests {r with compact = true }
  | _ -> error "use -compact"

let set_keep_chars () =
  match !op with
  | Translate_tests r ->
    op  := Translate_tests {r with style = Chars }
  | _ -> error "use -keep-chars"

let set_keep_lines () =
  match !op with
  | Translate_tests r ->
    op  := Translate_tests {r with style = Lines }
  | _ -> error "use -keep-lines"

let add_file_to_test filename =
  match !op with
  | Run_tests r ->
    op  := Run_tests {r with files_to_test = r.files_to_test @ [filename] }
  | _ -> error "test file"

let commandline_options =
[
  ("-e", Arg.Unit set_log_to_stderr, " Log to stderr instead of a file.");
  ("-promote", Arg.Unit set_promote,
   " Overwrite reference files with the test output (experimental, unstable)");
  ("-show-actions", Arg.Unit show_actions, " Show available actions.");
  ("-show-tests", Arg.Unit show_tests, " Show available tests.");
  ("-show-variables", Arg.Unit show_variables, " Show available variables.");
  ("-show-timings", Arg.Unit set_show_timings,
   " Show the wall clock time taken for each test file.");
  ("-timeout", Arg.Int set_timeout,
   "<seconds> Set maximal execution time for every command (in seconds)");
  ("-find-test-dirs", Arg.String set_find_test_dirs,
   " Find directories that contain tests (recursive).");
  ("-list-tests", Arg.String set_list_tests,
   " List tests in given directory.");
  ("-keep-test-dir-on-success", Arg.Unit set_keep_test_dir_on_success,
   " Keep the test directory (with the generated test artefacts) on success.");
  ("-translate", Arg.Unit set_translate,
   " Translate the test script from old to new syntax");
  ("-compact", Arg.Unit set_compact,
   " If translating, output the new script in compact mode.");
  ("-keep-lines", Arg.Unit set_keep_lines,
   " If translating, preserve line numbers in the output.");
  ("-keep-chars", Arg.Unit set_keep_chars,
   " If translating, preserve char offsets in the output.");
]

let usage = "Usage: " ^ Sys.argv.(0) ^ " options files to test"

let print_usage () =
  Printf.printf "%s\n%!" usage

let check_consistency = function
  | Run_tests { files_to_test = []; _ } -> print_usage(); exit 1
  | _ -> ()

let init_files_to_skip = function
  | Run_tests r ->
    Run_tests { r with
      files_to_skip = String.words (Sys.safe_getenv "OCAMLTEST_SKIP_FILESS")}
  | _ as op -> op    

let parse_commandline () =
  Arg.parse (Arg.align commandline_options) add_file_to_test usage;
  check_consistency !op;
  op := init_files_to_skip !op;
  !op
