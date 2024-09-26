(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Sebastien Hinderer, Tarides, Paris                   *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Implementation of the abstract debuggers description module *)

type t = {
  name : string;
  default_flags : string;
  script_flag : string;
}

let make :
  ~name : string ->
  ~default_flags : string ->
  ~script_flag : string

let make_run_action debugger =

let make_has_action debugger =
