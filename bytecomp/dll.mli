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

type dll_mode =
  | For_checking     (* will just check existence of symbols;
                        no need to do full symbol resolution *)
  | For_execution    (* will call functions from this DLL;
                        must resolve symbols completely *)

(* Open a list of DLLs.  First argument indicates whether to perform
   full symbol resolution.  Raise [Failure msg] in case of error. *)
val open_dlls: dll_mode -> string list -> unit

(* Close all DLLs *)
val close_all_dlls: unit -> unit

(* The abstract type representing C function pointers *)
type dll_address = CamlinternalDynlink.MiniDll.dll_address

type primitive_address = CamlinternalDynlink.MiniDll.primitive_address

(* Find a primitive in the currently opened DLLs and return its address.
   Return [None] if the primitive is not found. *)
val find_primitive: string -> primitive_address option

(* If linking in core (dynlink or toplevel), synchronize the VM
   table of primitive with the linker's table of primitive
   by storing the given primitive function at the given position
   in the VM table of primitives.  *)
val synchronize_primitive: int -> dll_address -> unit

(* Add the given directories at the head of the search path for DLLs *)
val add_path: string list -> unit

(* Remove the given directories from the search path for DLLs *)
val remove_path: string list -> unit

(* Initialization for separate compilation.
   Initialize the DLL search path to the directories given in the
   environment variable CAML_LD_LIBRARY_PATH, plus contents of ld.conf file
   if argument is [false].  If argument is [true], ignore ld.conf. *)
val init_compile: bool -> unit
