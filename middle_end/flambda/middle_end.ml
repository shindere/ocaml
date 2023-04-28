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

(* Implementation of the FLambda middle-end module *)

open Misc

type export_info = Export_info.t

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

let default_ui_export_info = Export_info.empty

let get_clambda_approx ui = assert false

let approx_for_global comp_unit =
  let id = Compilation_unit.get_persistent_ident comp_unit in
  if (Compilation_unit.equal
      predefined_exception_compilation_unit
      comp_unit)
     || Ident.is_predef id
     || not (Ident.global id)
  then invalid_arg (Format.asprintf "approx_for_global %a" Ident.print id);
  let modname = Ident.name id in
  match Hashtbl.find export_infos_table modname with
  | otherwise -> Some otherwise
  | exception Not_found ->
    match get_global_info id with
    | None -> None
    | Some ui ->
      let exported = get_flambda_export_info ui in
      Hashtbl.add export_infos_table modname exported;
      merged_environment := Export_info.merge !merged_environment exported;
      Some exported

