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

(* Definition of a few OCaml-specific environment modifiers *)

open Ocamltest_stdlib
open Environments

let expect =
[
  Add (Builtin_variables.script,
    "bash ${OCAMLSRCDIR}/testsuite/tools/expect");
]

let principal =
[
  Append (Ocaml_variables.flags, " -principal ");
  Add (Ocaml_variables.compiler_directory_suffix, ".principal");
  Add (Ocaml_variables.compiler_reference_suffix, ".principal");
]

let wrap str = (" " ^ str ^ " ")

let make_library_modifier library directory =
[
  Append (Ocaml_variables.directories, (wrap directory));
  Append (Ocaml_variables.libraries, (wrap library));
  Append (Builtin_variables.ld_library_path, (wrap directory));
]

let compiler_subdir subdir =
  Filename.make_path (Ocamltest_config.ocamlsrcdir :: subdir)

let testing = make_library_modifier
  "testing" (compiler_subdir ["testsuite"; "lib"])

let unix = make_library_modifier
  "unix" (compiler_subdir ["otherlibs"; "unix"])

let str = make_library_modifier
  "str" (compiler_subdir ["otherlibs"; "str"])

let _ =
  register_modifiers "expect" expect;
  register_modifiers "principal" principal;
  register_modifiers "testing" testing;
  register_modifiers "unix" unix;
  register_modifiers "str" str
