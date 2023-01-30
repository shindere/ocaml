(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Sebastien Hinderer, Tarides, Paris                   *)
(*                                                                        *)
(*   Copyright 2022 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Definition (and support tools) for dynamically loadalbe flies *)

(** {1 Long strings} *)

(** ``Long strings'' are mutable arrays of characters that are not limited
    in length to {!Sys.max_string_length}. *)

module LongString :
  sig
    type t = bytes array
    val create : int -> t
    val length : t -> int
    val get : t -> int -> char
    val set : t -> int -> char -> unit
    val blit : t -> int -> t -> int -> int -> unit
    val blit_string : string -> int -> t -> int -> int -> unit
    val output : out_channel -> t -> int -> int -> unit
    val input_bytes_into : t -> in_channel -> int -> unit
    val input_bytes : in_channel -> int -> t
  end

val load_compunit_code : in_channel -> int -> LongString.t

(* This needs to be kept in sync with the modules in the
   file_formats directroy *)

type modname = string
type crcs = (modname * Digest.t option) list

val magic_length : int
(** Length of magic number *)

val cmo_magic_number : string

(* Relocation information *)

type reloc_info =
    Reloc_literal of Obj.t                  (* structured constant *)
  | Reloc_getpredef of string               (* reference to a predef *)
  | Reloc_getglobal of string               (* reference to a global *)
  | Reloc_setglobal of string               (* definition of a global *)
  | Reloc_primitive of string               (* C primitive number *)

(* Descriptor for compilation units *)

type compilation_unit =
  { cu_name: modname;                   (* Name of compilation unit *)
    mutable cu_pos: int;                (* Absolute position in file *)
    cu_codesize: int;                   (* Size of code block *)
    cu_reloc: (reloc_info * int) list;  (* Relocation information *)
    cu_imports: crcs;                   (* Names and CRC of intfs imported *)
    cu_required_globals: string list;   (* Compilation units whose
                                           initialization side effects
                                           must occur before this one. *)
    cu_primitives: string list;         (* Primitives declared inside *)
    mutable cu_force_link: bool;        (* Must be linked even if unref'ed *)
    mutable cu_debug: int;              (* Position of debugging info, or 0 *)
    cu_debugsize: int }                 (* Length of debugging info *)

(* Format of a .cmo file:
     magic number (Config.cmo_magic_number)
     absolute offset of compilation unit descriptor
     block of relocatable bytecode
     debugging information if any
     compilation unit descriptor *)

val cma_magic_number : string

(* Descriptor for libraries *)

type library =
  { lib_units: compilation_unit list;   (* List of compilation units *)
    lib_custom: bool;                   (* Requires custom mode linking? *)
    (* In the following fields the lists are reversed with respect to
       how they end up being used on the command line. *)
    lib_ccobjs: string list;            (* C object files needed for -custom *)
    lib_ccopts: string list;            (* Extra opts to C compiler *)
    lib_dllibs: string list }           (* DLLs needed *)

(* Format of a .cma file:
     magic number (Config.cma_magic_number)
     absolute offset of library descriptor
     object code for first library member
     ...
     object code for last library member
     library descriptor *)

(* The CMXS format, keep in sync with file_formats/cmxs_format.mli *)

val cmxs_magic_number : string

(* Each .cmxs dynamically-loaded plugin contains a symbol
   "caml_plugin_header" containing the following info
   (as an externed record) *)

type dynunit = {
  dynu_name: modname;
  dynu_crc: Digest.t;
  dynu_imports_cmi: crcs;
  dynu_imports_cmx: crcs;
  dynu_defines: string list;
}

type dynheader = {
  dynu_magic: string;
  dynu_units: dynunit list;
}

val fatal_error: string -> 'a

(* Extract the name of a DLL from its external name (xxx.so or -lxxx) *)
val extract_dll_name: string -> string
