(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2001 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Handling of dynamically-linked libraries *)

type dll_handle
type dll_address = CamlinternalDynlink.MiniDll.dll_address
type dll_mode = For_checking | For_execution

external dll_open: string -> dll_handle = "caml_dynlink_open_lib"
external dll_close: dll_handle -> unit = "caml_dynlink_close_lib"
external dll_sym: dll_handle -> string -> dll_address
                = "caml_dynlink_lookup_symbol"
         (* returned dll_address may be Val_unit *)
external add_primitive: dll_address -> int = "caml_dynlink_add_primitive"
(* Current search path for DLLs *)
let search_path = ref ([] : string list)

type opened_dll =
  | Checking of Binutils.t
  | Execution of dll_handle

let dll_close = function
  | Checking _ -> ()
  | Execution dll -> dll_close dll

(* DLLs currently opened *)
let opened_dlls = ref ([] : (string * opened_dll) list)

(* Add the given directories to the search path for DLLs. *)
let add_path dirs =
  search_path := dirs @ !search_path

let remove_path dirs =
  search_path := List.filter (fun d -> not (List.mem d dirs)) !search_path

(* Open a list of DLLs, adding them to opened_dlls.
   Raise [Failure msg] in case of error. *)

let open_dll mode name =
  let name = name ^ Config.ext_dll in
  let fullname =
    try
      let fullname = Misc.find_in_path !search_path name in
      if Filename.is_implicit fullname then
        Filename.concat Filename.current_dir_name fullname
      else fullname
    with Not_found -> name in
  match List.assoc_opt fullname !opened_dlls, mode with
  | Some (Execution _), (For_execution | For_checking) -> ()
  | Some (Checking _), For_checking -> ()
  | None, For_checking ->
      begin match Binutils.read fullname with
      | Ok t -> opened_dlls := (fullname, Checking t) :: !opened_dlls
      | Error err ->
          failwith (fullname ^ ": " ^ Binutils.error_to_string err)
      end
  | (None | Some (Checking _) as current), For_execution ->
      begin match dll_open fullname with
      | dll ->
          let opened = match current with
            | None -> List.remove_assoc fullname !opened_dlls
            | Some _ -> !opened_dlls
          in
          opened_dlls := (fullname, Execution dll) :: opened
      | exception Failure msg ->
          failwith (fullname ^ ": " ^ msg)
      end

let open_dlls mode names =
  List.iter (open_dll mode) names

(* Close all DLLs *)

let close_all_dlls () =
  List.iter (fun (_, dll) -> dll_close dll) !opened_dlls;
  opened_dlls := [];

(* Find a primitive in the currently opened DLLs. *)

type primitive_address = CamlinternalDynlink.MiniDll.primitive_address

let find_primitive prim_name =
  let rec find seen = function
    [] ->
      None
  | (_,Execution dll) as curr :: rem ->
      let addr = dll_sym dll prim_name in
      if addr == Obj.magic () then find (curr :: seen) rem else begin
        if seen <> [] then opened_dlls := curr :: List.rev_append seen rem;
        Some (CamlinternalDynlink.MiniDll.Prim_loaded addr)
      end
  | (_,Checking t) as curr :: rem ->
      if Binutils.defines_symbol t prim_name then
        Some CamlinternalDynlink.MiniDll.Prim_exists
      else
        find (curr :: seen) rem
  in
  find [] !opened_dlls

(* If linking in core (dynlink or toplevel), synchronize the VM
   table of primitive with the linker's table of primitive
   by storing the given primitive function at the given position
   in the VM table of primitives.  *)

let linking_in_core = ref false

let synchronize_primitive num symb =
  if !linking_in_core then begin
    let actual_num = add_primitive symb in
    assert (actual_num = num)
  end

(* Read the [ld.conf] file and return the corresponding list of directories *)

let ld_conf_contents () =
  let path = ref [] in
  begin try
    let ic = open_in (Filename.concat Config.standard_library "ld.conf") in
    begin try
      while true do
        path := input_line ic :: !path
      done
    with End_of_file -> ()
    end;
    close_in ic
  with Sys_error _ -> ()
  end;
  List.rev !path

(* Initialization for separate compilation *)

let init_compile nostdlib =
  search_path :=
    CamlinternalDynlink.MiniDll.ld_library_path_contents() @
    (if nostdlib then [] else ld_conf_contents())
