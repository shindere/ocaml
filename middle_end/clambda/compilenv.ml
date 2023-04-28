(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2010 Institut National de Recherche en Informatique et     *)
(*     en Automatique                                                     *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Compilation environments for compilation units *)

[@@@ocaml.warning "+a-4-9-40-41-42"]

open Config
open Cmx_format
open Middle_end

type error =
    Not_a_unit_info of string
  | Corrupted_unit_info of string
  | Illegal_renaming of string * string * string
  | Mismatching_for_pack of string * string * string * string option

exception Error of error

let not_implemented function_name = fun _ ->
  failwith
    ("The " ^ function_name ^
    " function has been called but is not implemented for the CLambda \
      middle-end")

let global_infos_table =
  (Hashtbl.create 17 : (string, unit_infos option) Hashtbl.t)

module CstMap =
  Map.Make(struct
    type t = Clambda.ustructured_constant
    let compare = Clambda.compare_structured_constants
    (* PR#6442: it is incorrect to use Stdlib.compare on values of type t
       because it compares "0.0" and "-0.0" equal. *)
  end)

module SymMap = Misc.Stdlib.String.Map

type structured_constants =
  {
    strcst_shared: string CstMap.t;
    strcst_all: Clambda.ustructured_constant SymMap.t;
  }

let structured_constants_empty  =
  {
    strcst_shared = CstMap.empty;
    strcst_all = SymMap.empty;
  }

let structured_constants = ref structured_constants_empty


let exported_constants = Hashtbl.create 17

let default_ui_export_info = Clambda.Value_unknown

let current_unit =
  { ui_name = "";
    ui_symbol = "";
    ui_defines = [];
    ui_imports_cmi = [];
    ui_imports_cmx = [];
    ui_curry_fun = [];
    ui_apply_fun = [];
    ui_send_fun = [];
    ui_force_link = false;
    ui_export_info = default_ui_export_info;
    ui_for_pack = None }

let concat_symbol unitname id =
  unitname ^ "." ^ id

let symbolname_for_pack pack name =
  match pack with
  | None -> name
  | Some p -> concat_symbol p name

let unit_id_from_name name = Ident.create_persistent name

let make_symbol ?(unitname = current_unit.ui_symbol) idopt =
  let prefix = "caml" ^ unitname in
  match idopt with
  | None -> prefix
  | Some id -> concat_symbol prefix id

let current_unit_linkage_name = not_implemented "current_unit_linkage_name"

let reset ?packname name =
  Hashtbl.clear global_infos_table;
  let symbol = symbolname_for_pack packname name in
  current_unit.ui_name <- name;
  current_unit.ui_symbol <- symbol;
  current_unit.ui_defines <- [symbol];
  current_unit.ui_imports_cmi <- [];
  current_unit.ui_imports_cmx <- [];
  current_unit.ui_curry_fun <- [];
  current_unit.ui_apply_fun <- [];
  current_unit.ui_send_fun <- [];
  current_unit.ui_force_link <- !Clflags.link_everything;
  current_unit.ui_for_pack <- packname;
  Hashtbl.clear exported_constants;
  structured_constants := structured_constants_empty;
  current_unit.ui_export_info <- default_ui_export_info;
  let compilation_unit =
    Compilation_unit.create
      (Ident.create_persistent name)
      (current_unit_linkage_name ())
  in
  Compilation_unit.set_current compilation_unit

let current_unit_infos () =
  current_unit

let current_unit_name () =
  current_unit.ui_name

let symbol_in_current_unit name =
  let prefix = "caml" ^ current_unit.ui_symbol in
  name = prefix ||
  (let lp = String.length prefix in
   String.length name >= 2 + lp
   && String.sub name 0 lp = prefix
   && name.[lp] = '_'
   && name.[lp + 1] = '_')

let read_unit_info filename =
  let ic = open_in_bin filename in
  try
    let buffer = really_input_string ic (String.length cmx_magic_number) in
    if buffer <> cmx_magic_number then begin
      close_in ic;
      raise(Error(Not_a_unit_info filename))
    end;
    let ui = (input_value ic : unit_infos) in
    let crc = Digest.input ic in
    close_in ic;
    (ui, crc)
  with End_of_file | Failure _ ->
    close_in ic;
    raise(Error(Corrupted_unit_info(filename)))

let read_library_info filename =
  let ic = open_in_bin filename in
  let buffer = really_input_string ic (String.length cmxa_magic_number) in
  if buffer <> cmxa_magic_number then
    raise(Error(Not_a_unit_info filename));
  let infos = (input_value ic : library_infos) in
  close_in ic;
  infos


(* Read and cache info on global identifiers *)

let get_global_info global_ident = (
  let modname = Ident.name global_ident in
  if modname = current_unit.ui_name then
    Some current_unit
  else begin
    try
      Hashtbl.find global_infos_table modname
    with Not_found ->
      let (infos, crc) =
        if Env.is_imported_opaque modname then (None, None)
        else begin
          try
            let filename =
              Load_path.find_uncap (modname ^ ".cmx") in
            let (ui, crc) = read_unit_info filename in
            if ui.ui_name <> modname then
              raise(Error(Illegal_renaming(modname, ui.ui_name, filename)));
            (* Linking to a compilation unit expected to go into a
               pack (ui_for_pack = Some ...) is possible only from
               inside the same pack, but it is perfectly ok to link to
               an unit outside of the pack. *)
            (match ui.ui_for_pack, current_unit.ui_for_pack with
             | None, _ -> ()
             | Some p1, Some p2 when String.equal p1 p2 -> ()
             | Some p1, p2 ->
               raise (Error (Mismatching_for_pack
                               (filename, p1, current_unit.ui_name, p2))));
            (Some ui, Some crc)
          with Not_found ->
            let warn = Warnings.No_cmx_file modname in
              Location.prerr_warning Location.none warn;
              (None, None)
          end
      in
      current_unit.ui_imports_cmx <-
        (modname, crc) :: current_unit.ui_imports_cmx;
      Hashtbl.add global_infos_table modname infos;
      infos
  end
)

let cache_unit_info ui =
  Hashtbl.add global_infos_table ui.ui_name (Some ui)

(* Return the approximation of a global identifier *)

let get_clambda_approx ui = ui.ui_export_info

let toplevel_approx :
  (string, Clambda.value_approximation) Hashtbl.t = Hashtbl.create 16

let record_global_approx_toplevel () =
  Hashtbl.add toplevel_approx current_unit.ui_name
    (get_clambda_approx current_unit)

let global_approx id =
  if Ident.is_predef id then Clambda.Value_unknown
  else try Hashtbl.find toplevel_approx (Ident.name id)
  with Not_found ->
    match get_global_info id with
      | None -> Clambda.Value_unknown
      | Some ui -> get_clambda_approx ui

(* Return the symbol used to refer to a global identifier *)

let symbol_for_global = not_implemented "symbol_for_global"

(* Register the approximation of the module being compiled *)

let symbol_for_global' = not_implemented "symbol_for_global'"

let set_global_approx approx =
  current_unit.ui_export_info <- approx

(* Exporting and importing cross module information *)

let approx_env = not_implemented "approx_env"

(* Record that a currying function or application function is needed *)

let need_curry_fun n =
  if not (List.mem n current_unit.ui_curry_fun) then
    current_unit.ui_curry_fun <- n :: current_unit.ui_curry_fun

let need_apply_fun n =
  assert(n > 0);
  if not (List.mem n current_unit.ui_apply_fun) then
    current_unit.ui_apply_fun <- n :: current_unit.ui_apply_fun

let need_send_fun n =
  if not (List.mem n current_unit.ui_send_fun) then
    current_unit.ui_send_fun <- n :: current_unit.ui_send_fun

(* Write the description of the current unit *)

let write_unit_info info filename =
  let oc = open_out_bin filename in
  output_string oc cmx_magic_number;
  output_value oc info;
  flush oc;
  let crc = Digest.file filename in
  Digest.output oc crc;
  close_out oc

let save_unit_info filename =
  current_unit.ui_imports_cmi <- Env.imports();
  write_unit_info current_unit filename

let current_unit = not_implemented "current_unit"

let current_unit_symbol = not_implemented "current_unit_symbol"

let const_label = ref 0

let new_const_symbol () =
  incr const_label;
  make_symbol (Some (Int.to_string !const_label))

let snapshot () = !structured_constants
let backtrack s = structured_constants := s

let new_structured_constant cst ~shared =
  let {strcst_shared; strcst_all} = !structured_constants in
  if shared then
    try
      CstMap.find cst strcst_shared
    with Not_found ->
      let lbl = new_const_symbol() in
      structured_constants :=
        {
          strcst_shared = CstMap.add cst lbl strcst_shared;
          strcst_all = SymMap.add lbl cst strcst_all;
        };
      lbl
  else
    let lbl = new_const_symbol() in
    structured_constants :=
      {
        strcst_shared;
        strcst_all = SymMap.add lbl cst strcst_all;
      };
    lbl

let add_exported_constant s =
  Hashtbl.replace exported_constants s ()

let clear_structured_constants () =
  structured_constants := structured_constants_empty

let structured_constant_of_symbol s =
  SymMap.find_opt s (!structured_constants).strcst_all

let structured_constants () =
  let provenance : Clambda.usymbol_provenance =
    { original_idents = [];
      module_path =
        Path.Pident (Ident.create_persistent (current_unit_name ()));
    }
  in
  SymMap.bindings (!structured_constants).strcst_all
  |> List.map
    (fun (symbol, definition) ->
       {
         Clambda.symbol;
         exported = Hashtbl.mem exported_constants symbol;
         definition;
         provenance = Some provenance;
       })

let closure_symbol fv =
  let compilation_unit = Closure_id.get_compilation_unit fv in
  let unitname =
    Linkage_name.to_string (Compilation_unit.get_linkage_name compilation_unit)
  in
  let linkage_name =
    concat_symbol unitname ((Closure_id.unique_name fv) ^ "_closure")
  in
  Symbol.of_global_linkage compilation_unit (Linkage_name.create linkage_name)

let require_global global_ident =
  if not (Ident.is_predef global_ident) then
    ignore (get_global_info global_ident : unit_infos option)

(* Error report *)

open Format

let report_error ppf = function
  | Not_a_unit_info filename ->
      fprintf ppf "%a@ is not a compilation unit description."
        Location.print_filename filename
  | Corrupted_unit_info filename ->
      fprintf ppf "Corrupted compilation unit description@ %a"
        Location.print_filename filename
  | Illegal_renaming(name, modname, filename) ->
      fprintf ppf "%a@ contains the description for unit\
                   @ %s when %s was expected"
        Location.print_filename filename name modname
  | Mismatching_for_pack(filename, pack_1, current_unit, None) ->
      fprintf ppf "%a@ was built with -for-pack %s, but the \
                   @ current unit %s is not"
        Location.print_filename filename pack_1 current_unit
  | Mismatching_for_pack(filename, pack_1, current_unit, Some pack_2) ->
      fprintf ppf "%a@ was built with -for-pack %s, but the \
                   @ current unit %s is built with -for-pack %s"
        Location.print_filename filename pack_1 current_unit pack_2

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
