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

(* Main program of the ocamltest test driver *)

open Operations
open Testfile

let ignored s =
  s = "" || s.[0] = '_' || s.[0] = '.'

let sort_strings = List.sort String.compare

let is_test = is_testfile

let find_test_dirs dir =
  let res = ref [] in
  let rec loop dir =
    let contains_tests = ref false in
    Array.iter (fun s ->
        if ignored s then ()
        else begin
          let s = dir ^ "/" ^ s in
          if Sys.is_directory s then loop s
          else if not !contains_tests && is_test s then contains_tests := true
        end
      ) (Sys.readdir dir);
    if !contains_tests then res := dir :: !res
  in
  loop dir;
  sort_strings !res

let list_tests dir =
  let res = ref [] in
  if Sys.is_directory dir then begin
    Array.iter (fun s ->
        if ignored s then ()
        else begin
          let s' = dir ^ "/" ^ s in
          if Sys.is_directory s' || not (is_test s') then ()
          else res := s :: !res
        end
      ) (Sys.readdir dir)
  end;
  sort_strings !res

let list_tests dir =
  match list_tests dir with
  | [] -> exit 1
  | res -> List.iter print_endline res

let find_test_dirs dir =
  List.iter print_endline (find_test_dirs dir)

let main () =
  match Options.parse_commandline () with
  | Run_tests settings ->
    List.iter (Testfile.run settings) settings.files_to_test
  | Find_test_dirs dirs -> List.iter find_test_dirs dirs
  | List_tests dirs -> List.iter list_tests dirs
  | Translate_tests settings ->
    List.iter (translate settings) settings.files_to_translate

let _ = main ()
