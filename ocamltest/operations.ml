(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Sebastien Hinderer, Tarides                      *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Description of the different operations ocamltest can perform *)

type test_settings =
{
  files_to_test : string list;
  log_to_stderr : bool;
  promote : bool;
  default_timeout : int;
  keep_test_dir_on_success : bool;
  show_timings : bool;
  files_to_skip : string list;
}

let default_test_settings =
{
  files_to_test = [];
  log_to_stderr = false;
  promote = false;
  default_timeout = 0;
  keep_test_dir_on_success = false;
  show_timings = false;
  files_to_skip = [];
}

type translate_settings =
{
  files_to_translate : string list;
  style : Translate.style;
  compact : bool;
}

let default_translate_settings =
{
  files_to_translate = [];
  style = Translate.Plain;
  compact = false;
}

type t =
  | Run_tests of test_settings
  | Find_test_dirs of string list (* Directories to scan for test dirs *)
  | List_tests of string list (* Directories to scan for test files *)
  | Translate_tests of translate_settings

let to_string = function
  | Run_tests _ -> "run tests"
  | Find_test_dirs _ -> "find test dirs"
  | List_tests _ -> "list tests"
  | Translate_tests _ -> "translate"
