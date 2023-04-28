(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Sebastien Hinderer, Tarides, Paris                   *)
(*                                                                        *)
(*   Copyright 2023 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Interface for the CLambda middle-end module *)

open Misc

type export_info = Clambda.value_approximation

type unit_infos =
  { mutable ui_name: modname;             (* Name of unit implemented *)
    mutable ui_symbol: string;            (* Prefix for symbols *)
    mutable ui_defines: string list;      (* Unit and sub-units implemented *)
    mutable ui_imports_cmi: crcs;         (* Interfaces imported *)
    mutable ui_imports_cmx: crcs;         (* Infos imported *)
    mutable ui_curry_fun: int list;       (* Currying functions needed *)
    mutable ui_apply_fun: int list;       (* Apply functions needed *)
    mutable ui_send_fun: int list;        (* Send functions needed *)
    mutable ui_export_info: export_info;
    mutable ui_force_link: bool;          (* Always linked *)
    mutable ui_for_pack: string option }  (* Part of a pack *)
