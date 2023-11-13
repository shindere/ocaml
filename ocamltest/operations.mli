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

val default_test_settings : test_settings

type translate_settings =
{
  files_to_translate : string list;
  style : Translate.style;
  compact : bool;
}

val default_translate_settings : translate_settings

type t =
  | Run_tests of test_settings
  | Find_test_dirs of string list (* Directories to scan for test dirs *)
  | List_tests of string list (* Directories to scan for test files *)
  | List_parallel_tests of string list
    (* Directories to scan for files whose tests require more than one core *)
  | List_sequential_tests of string list
    (* Directories to scan for files whose tests require only one core *)
  | Translate_tests of translate_settings

val to_string : t -> string
