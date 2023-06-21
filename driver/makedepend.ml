(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1999 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Parsetree
module String = Misc.Stdlib.String

let ppf = Format.err_formatter
(* Print the dependencies *)

type file_kind = ML | MLI | MLL | MLY

let ml_synonyms = ref [".ml"]
let mli_synonyms = ref [".mli"]
let mll_synonyms = ref [".mll"]
let mly_synonyms = ref [".mly"]
let shared = ref false
let native_only = ref false
let bytecode_only = ref false
let raw_dependencies = ref false
let sort_files = ref false
let all_dependencies = ref false
let nocwd = ref false
let one_line = ref false
let allow_approximation = ref false
let debug = ref false

(* [(dir, contents)] returned by [Sys.readdir dir]. *)
let load_path = ref ([] : (string * string array) list)
let files =
  ref ([] : (string * file_kind * String.Set.t * string list) list)
let module_map = ref String.Map.empty

module Error_occurred : sig
  val set : unit -> unit
  val get : unit -> bool
end = struct
  (* Once set to [true], [error_occurred] should never be set to
     [false]. *)
  let error_occurred = ref false
  let get () = !error_occurred
  let set () = error_occurred := true
end

(* Fix path to use '/' as directory separator instead of '\'.
   Only under Windows. *)
let fix_slash s =
  if Sys.os_type = "Unix" then s else begin
    String.map (function '\\' -> '/' | c -> c) s
  end

(* Since we reinitialize load_path after reading OCAMLCOMP,
  we must use a cache instead of calling Sys.readdir too often. *)
let dirs = ref String.Map.empty
let readdir dir =
  try
    String.Map.find dir !dirs
  with Not_found ->
    let contents =
      try
        Sys.readdir dir
      with Sys_error msg ->
        Format.fprintf ppf "@[Bad -I option: %s@]@." msg;
        Error_occurred.set ();
        [||]
    in
    dirs := String.Map.add dir contents !dirs;
    contents

let add_to_load_path dir =
  try
    let dir = Misc.expand_directory Config.standard_library dir in
    let contents = readdir dir in
    load_path := (dir, contents) :: !load_path
  with Sys_error msg ->
    Format.fprintf ppf "@[Bad -I option: %s@]@." msg;
    Error_occurred.set ()

let add_to_synonym_list synonyms suffix =
  if (String.length suffix) > 1 && suffix.[0] = '.' then
    synonyms := suffix :: !synonyms
  else begin
    Format.fprintf ppf "@[Bad suffix: '%s'@]@." suffix;
    Error_occurred.set ()
  end

(* Find file 'name' (capitalized) in search path *)
let find_module_in_load_path name =
  let synonyms = !mli_synonyms @ !ml_synonyms @ !mll_synonyms @ !mly_synonyms in
  let names = List.map (fun ext -> name ^ ext) synonyms
  and unames =
    let uname = String.uncapitalize_ascii name in
    List.map (fun ext -> uname ^ ext) synonyms
  in
  let rec find_in_path = function
    | [] -> raise Not_found
    | (dir, contents) :: rem ->
        let mem s = List.mem s names || List.mem s unames in
        match Array.find_opt mem contents with
        | Some truename ->
            if dir = "." then truename else Filename.concat dir truename
        | None -> find_in_path rem in
  find_in_path !load_path

let find_dependency target_kind modname (byt_deps, opt_deps) =
  match find_module_in_load_path modname with
  | exception Not_found -> (byt_deps, opt_deps)
  | filename ->
    let basename = Filename.chop_extension filename in
    let cmi_file = basename ^ ".cmi"
    and cmx_file = basename ^ ".cmx" in
    let mli_exists =
      List.exists (fun ext -> Sys.file_exists (basename ^ ext)) !mli_synonyms
    and ml_exists =
      List.exists (fun ext -> Sys.file_exists (basename ^ ext)) !ml_synonyms
    and mll_exists =
      List.exists (fun ext -> Sys.file_exists (basename ^ ext)) !mll_synonyms
    and mly_exists =
      List.exists (fun ext -> Sys.file_exists (basename ^ ext)) !mly_synonyms in
    if mli_exists || mll_exists || mly_exists then
      let new_opt_dep =
        if !all_dependencies then
          match target_kind with
          | MLI -> [ cmi_file ]
          | ML  ->
              cmi_file :: (if ml_exists then [ cmx_file ] else [])
          | MLL | MLY -> assert false
        else
        (* this is a make-specific hack that makes .cmx to be a 'proxy'
           target that would force the dependency on .cmi via transitivity *)
        if ml_exists  || mll_exists || mly_exists
        then [ cmx_file ]
        else [ cmi_file ]
      in
      ( cmi_file :: byt_deps, new_opt_dep @ opt_deps)
    else
      (* "just .ml" case *)
      let bytenames =
        if !all_dependencies then
          match target_kind with
          | MLI -> [ cmi_file ]
          | ML  -> [ cmi_file ]
          | MLL | MLY -> assert false
        else
          (* again, make-specific hack *)
          [basename ^ (if !native_only then ".cmx" else ".cmo")] in
      let optnames =
        if !all_dependencies
        then match target_kind with
          | MLI -> [ cmi_file ]
          | ML  -> [ cmi_file; cmx_file ]
          | MLL | MLY -> assert false
        else [ cmx_file ]
      in
      (bytenames @ byt_deps, optnames @  opt_deps)

let (depends_on, escaped_eol) = (":", " \\\n    ")

let print_filename s =
  let s = if !Clflags.force_slash then fix_slash s else s in
  if not (String.contains s ' ') then begin
    print_string s;
  end else begin
    let rec count n i =
      if i >= String.length s then n
      else if s.[i] = ' ' then count (n+1) (i+1)
      else count n (i+1)
    in
    let spaces = count 0 0 in
    let result = Bytes.create (String.length s + spaces) in
    let rec loop i j =
      if i >= String.length s then ()
      else if s.[i] = ' ' then begin
        Bytes.set result j '\\';
        Bytes.set result (j+1) ' ';
        loop (i+1) (j+2);
      end else begin
        Bytes.set result j s.[i];
        loop (i+1) (j+1);
      end
    in
    loop 0 0;
    print_bytes result;
  end

let print_dependencies target_files deps =
  let pos = ref 0 in
  let print_on_same_line item =
    if !pos <> 0 then print_string " ";
    print_filename item;
    pos := !pos + String.length item + 1;
  in
  let print_on_new_line item =
    print_string escaped_eol;
    print_filename item;
    pos := String.length item + 4;
  in
  let print_compact item =
    if !one_line || (!pos + 1 + String.length item <= 77)
    then print_on_same_line item
    else print_on_new_line item
  in
  let print_dep item =
    if !one_line
    then print_on_same_line item
    else print_on_new_line item
  in
  List.iter print_compact target_files;
  print_string " "; print_string depends_on;
  pos := !pos + String.length depends_on + 1;
  List.iter print_dep deps;
  print_string "\n"

let print_raw_dependencies source_file deps =
  print_filename source_file; print_string depends_on;
  String.Set.iter
    (fun dep ->
       (* filter out "*predef*" *)
      if (String.length dep > 0)
          && (match dep.[0] with
              | 'A'..'Z' | '\128'..'\255' -> true
              | _ -> false) then
        begin
          print_char ' ';
          print_string dep
        end)
    deps;
  print_char '\n'


(* Process one file *)

let print_exception exn =
  Location.report_exception ppf exn

let report_err exn =
  Error_occurred.set ();
  print_exception exn

let tool_name = "ocamldep"

let rec lexical_approximation lexbuf =
  (* Approximation when a file can't be parsed.
     Heuristic:
     - first component of any path starting with an uppercase character is a
       dependency.
     - always skip the token after a dot, unless dot is preceded by a
       lower-case identifier
     - always skip the token after a backquote
  *)
  try
    let rec process ~after_lident lexbuf =
      match Lexer.token lexbuf with
      | Parser.UIDENT name ->
          Depend.free_structure_names :=
            String.Set.add name !Depend.free_structure_names;
          process ~after_lident:false lexbuf
      | Parser.LIDENT _ -> process ~after_lident:true lexbuf
      | Parser.DOT when after_lident -> process ~after_lident:false lexbuf
      | Parser.DOT | Parser.BACKQUOTE -> skip_one lexbuf
      | Parser.EOF -> ()
      | _ -> process ~after_lident:false lexbuf
    and skip_one lexbuf =
      match Lexer.token lexbuf with
      | Parser.DOT | Parser.BACKQUOTE -> skip_one lexbuf
      | Parser.EOF -> ()
      | _ -> process ~after_lident:false lexbuf

    in
    process ~after_lident:false lexbuf
  with Lexer.Error _ -> lexical_approximation lexbuf

let read_and_approximate inputfile =
  Depend.free_structure_names := String.Set.empty;
  In_channel.with_open_bin inputfile @@ begin fun ic ->
  try
    seek_in ic 0;
    Location.input_name := inputfile;
    let lexbuf = Lexing.from_channel ic in
    Location.init lexbuf inputfile;
    lexical_approximation lexbuf
  with exn ->
    report_err exn
  end;
  !Depend.free_structure_names

let read_parse_and_extract parse_function extract_function def ast_kind
    source_file =
  Depend.pp_deps := [];
  Depend.free_structure_names := String.Set.empty;
  try
    let input_file = Pparse.preprocess source_file in
    begin try
      let ast = Pparse.file ~tool_name input_file parse_function ast_kind in
      let bound_vars =
        List.fold_left
          (fun bv modname ->
             let lid =
               let lexbuf = Lexing.from_string modname in
               Location.init lexbuf
                 (Printf.sprintf "command line argument: -open %S" modname);
               Parse.simple_module_path lexbuf in
             Depend.open_module bv lid)
          !module_map ((* PR#7248 *) List.rev !Clflags.open_modules)
      in
      let r = extract_function bound_vars ast in
      Pparse.remove_preprocessed input_file;
      (!Depend.free_structure_names, r)
    with x ->
      Pparse.remove_preprocessed input_file;
      raise x
    end
  with x -> begin
    print_exception x;
    if not !allow_approximation then begin
      Error_occurred.set ();
      (String.Set.empty, def)
    end else
      (read_and_approximate source_file, def)
  end

let print_ml_dependencies (source_file, kind, extracted_deps, pp_deps) =
  let basename = Filename.chop_extension source_file in
  let byte_targets = [ basename ^ ".cmo" ] in
  let native_targets =
    if !all_dependencies
    then [ basename ^ ".cmx"; basename ^ ".o" ]
    else [ basename ^ ".cmx" ] in
  let shared_targets = [ basename ^ ".cmxs" ] in
  let init_deps =
    if !all_dependencies || kind = MLL || kind = MLY
    then [source_file] else [] in
  let cmi_name = basename ^ ".cmi" in
  let init_deps, extra_targets =
    if List.exists (fun ext -> Sys.file_exists (basename ^ ext))
        !mli_synonyms
    then (cmi_name :: init_deps, cmi_name :: init_deps), []
    else (init_deps, init_deps),
         (if !all_dependencies then [cmi_name] else [])
  in
  let (byt_deps, native_deps) =
    String.Set.fold (find_dependency ML)
      extracted_deps init_deps in
  if not !native_only then
    print_dependencies (byte_targets @ extra_targets) (byt_deps @ pp_deps);
  if not !bytecode_only then
    begin
      print_dependencies (native_targets @ extra_targets)
        (native_deps @ pp_deps);
      if !shared then
        print_dependencies (shared_targets @ extra_targets)
          (native_deps @ pp_deps)
    end

let print_mli_dependencies source_file extracted_deps pp_deps =
  let basename = Filename.chop_extension source_file in
  let (byt_deps, _opt_deps) =
    String.Set.fold (find_dependency MLI)
      extracted_deps ([], []) in
  print_dependencies [basename ^ ".cmi"] (byt_deps @ pp_deps)

let print_file_dependencies ((source_file, kind, extracted_deps, pp_deps) as args) =
  if !raw_dependencies then begin
    print_raw_dependencies source_file extracted_deps
  end else
    match kind with
    | ML -> print_ml_dependencies args
    | MLI -> print_mli_dependencies source_file extracted_deps pp_deps
    | MLL -> print_ml_dependencies args
    | MLY -> print_ml_dependencies args


let ml_file_dependencies source_file =
  let parse_use_file_as_impl lexbuf =
    let f x =
      match x with
      | Ptop_def s -> s
      | Ptop_dir _ -> []
    in
    List.concat_map f (Parse.use_file lexbuf)
  in
  let (extracted_deps, ()) =
    read_parse_and_extract parse_use_file_as_impl Depend.add_implementation ()
                           Pparse.Structure source_file
  in
  files := (source_file, ML, extracted_deps, !Depend.pp_deps) :: !files

let mli_file_dependencies source_file =
  let (extracted_deps, ()) =
    read_parse_and_extract Parse.interface Depend.add_signature ()
                           Pparse.Signature source_file
  in
  files := (source_file, MLI, extracted_deps, !Depend.pp_deps) :: !files

let process_ocaml_fragments buf ~need_struct_item action start_pos end_pos
      (extracted_deps, pp_deps) =
  let header_size = Buffer.length buf in
  let size = end_pos - start_pos in
  if need_struct_item then
    Buffer.add_string buf "let _ =\n";
  action start_pos size;
  let filename = Filename.temp_file "ocamldep-" ".ml" in
  Misc.protect_writing_to_file ~filename ~f:(fun oc ->
      output_string oc (Buffer.contents buf));
  ml_file_dependencies filename;
  Misc.remove_file filename;
  Buffer.truncate buf header_size;
  match !files with
  | (_, ML, extracted_deps', pp_deps') :: tl ->
     files := tl;
     (String.Set.union extracted_deps' extracted_deps),
     (pp_deps' @ pp_deps)
  | _ -> assert false

let mll_file_dependencies source_file =
  let mll = In_channel.with_open_bin source_file Misc.string_of_file in
  let lexbuf = Lexing.from_string mll in
  let lexdef = Lex_parser.lexer_definition Lex_lexer.main lexbuf in
  let buf = Buffer.create 4096 in
  let header_size = lexdef.header.end_pos - lexdef.header.start_pos in
  Buffer.add_substring buf mll lexdef.header.start_pos header_size;
  let action pos len = Buffer.add_substring buf mll pos len in
  let deps =
    List.fold_left (fun deps { Syntax.clauses; _ } ->
        List.fold_left (fun deps (_, entry) ->
            let { Syntax.start_pos; end_pos; _ } = entry in
            process_ocaml_fragments buf ~need_struct_item:true action start_pos
              end_pos deps
          ) deps clauses
      ) (String.Set.empty, []) lexdef.entrypoints
  in
  let extracted_deps, pp_deps =
    process_ocaml_fragments buf ~need_struct_item:false action
      lexdef.trailer.start_pos lexdef.trailer.end_pos deps in
  files := (source_file, MLY, extracted_deps, pp_deps) :: !files

(* This function doesn't process token type payload. The module is
   likely referred to in the semantic actions. *)
let mly_file_dependencies source_file =
  let mly = In_channel.with_open_bin source_file Misc.string_of_file in
  let lexbuf = Lexing.from_string mly in
  let buf = Buffer.create 4096 in
  let action pos len =
    String.sub mly pos len
    (* The sigil for ocamlyacc variables isn't valid OCaml syntax. *)
    |> String.map (function '$' -> '_' | c -> c)
    |> Buffer.add_string buf
  in
  let rec aux deps =
    match Ocamldzo.main lexbuf with
    | Theader (start, finish) ->
       let header_size = finish.Lexing.pos_cnum - start.Lexing.pos_cnum in
       Buffer.add_substring buf mly start.Lexing.pos_cnum header_size;
       aux deps
    | Taction (start, finish) ->
       let start_pos, end_pos = start.Lexing.pos_cnum, finish.Lexing.pos_cnum in
       process_ocaml_fragments buf ~need_struct_item:true action start_pos
         end_pos deps
    | Ttrailer (start, finish) ->
       let start_pos, end_pos = start.Lexing.pos_cnum, finish.Lexing.pos_cnum in
       process_ocaml_fragments buf ~need_struct_item:false action start_pos
         end_pos deps
    | Teof -> deps
  in
  let extracted_deps, pp_deps = aux (String.Set.empty, []) in
  files := (source_file, MLY, extracted_deps, pp_deps) :: !files

let process_file_as process_fun def source_file =
  Compenv.readenv ppf (Before_compile source_file);
  load_path := [];
  let cwd = if !nocwd then [] else [Filename.current_dir_name] in
  List.iter add_to_load_path (
      (!Compenv.last_include_dirs @
       !Clflags.include_dirs @
       !Compenv.first_include_dirs @
       cwd
      ));
  Location.input_name := source_file;
  try
    if Sys.file_exists source_file then process_fun source_file else def
  with x -> report_err x; def

let process_file source_file ~ml_file ~mli_file ~mll_file ~mly_file ~def =
  if List.exists (Filename.check_suffix source_file) !ml_synonyms then
    process_file_as ml_file def source_file
  else if List.exists (Filename.check_suffix source_file) !mli_synonyms then
    process_file_as mli_file def source_file
  else if List.exists (Filename.check_suffix source_file) !mll_synonyms then
    process_file_as mll_file def source_file
  else if List.exists (Filename.check_suffix source_file) !mly_synonyms then
    process_file_as mly_file def source_file
  else def

let file_dependencies source_file =
  process_file source_file ~def:()
    ~ml_file:ml_file_dependencies
    ~mli_file:mli_file_dependencies
    ~mll_file:mll_file_dependencies
    ~mly_file:mly_file_dependencies

let file_dependencies_as kind =
  match kind with
  | ML -> process_file_as ml_file_dependencies ()
  | MLI -> process_file_as mli_file_dependencies ()
  | MLL -> process_file_as mll_file_dependencies ()
  | MLY -> process_file_as mly_file_dependencies ()

let sort_files_by_dependencies files =
  let h = Hashtbl.create 31 in
  let worklist = ref [] in

(* Init Hashtbl with all defined modules *)
  let files = List.map (fun (file, file_kind, deps, pp_deps) ->
    let modname =
      String.capitalize_ascii (Filename.chop_extension (Filename.basename file))
    in
    let key = (modname, file_kind) in
    let new_deps = ref [] in
    Hashtbl.add h key (file, new_deps);
    worklist := key :: !worklist;
    (modname, file_kind, deps, new_deps, pp_deps)
  ) files in

(* Keep only dependencies to defined modules *)
  List.iter (fun (modname, file_kind, deps, new_deps, _pp_deps) ->
    let add_dep modname kind =
      new_deps := (modname, kind) :: !new_deps;
    in
    String.Set.iter (fun modname ->
      match file_kind with
        | ML -> (* ML depends both on ML and MLI *)
            if Hashtbl.mem h (modname, MLI) then add_dep modname MLI;
            if Hashtbl.mem h (modname, ML) then add_dep modname ML
        | MLI -> (* MLI depends on MLI if exists, or ML otherwise *)
          if Hashtbl.mem h (modname, MLI) then add_dep modname MLI
          else if Hashtbl.mem h (modname, ML) then add_dep modname ML
        | MLL | MLY -> ()
    ) deps;
    if file_kind = ML then (* add dep from .ml to .mli *)
      if Hashtbl.mem h (modname, MLI) then add_dep modname MLI
  ) files;

(* Print and remove all files with no remaining dependency. Iterate
   until all files have been removed (worklist is empty) or
   no file was removed during a turn (cycle). *)
  let printed = ref true in
  while !printed && !worklist <> [] do
    let files = !worklist in
    worklist := [];
    printed := false;
    List.iter (fun key ->
      let (file, deps) = Hashtbl.find h key in
      let set = !deps in
      deps := [];
      List.iter (fun key ->
        if Hashtbl.mem h key then deps := key :: !deps
      ) set;
      if !deps = [] then begin
        printed := true;
        Printf.printf "%s " file;
        Hashtbl.remove h key;
      end else
        worklist := key :: !worklist
    ) files
  done;

  if !worklist <> [] then begin
    Location.error "cycle in dependencies. End of list is not sorted."
    |> Location.print_report ppf;
    let sorted_deps =
      let li = ref [] in
      Hashtbl.iter (fun _ file_deps -> li := file_deps :: !li) h;
      List.sort (fun (file1, _) (file2, _) -> String.compare file1 file2) !li
    in
    List.iter (fun (file, deps) ->
      Format.fprintf ppf "\t@[%s: " file;
      List.iter (fun (modname, kind) ->
        Format.fprintf ppf "%s.%s " modname
          (match kind with ML -> "ml" | MLI -> "mli" | MLL -> "mll"
                           | MLY -> "mly")
      ) !deps;
      Format.fprintf ppf "@]@.";
      Printf.printf "%s " file) sorted_deps;
    Error_occurred.set ()
  end;
  Printf.printf "\n%!";
  ()

(* Map *)

let rec dump_map s0 ppf m =
  let open Depend in
  String.Map.iter
    (fun key (Node(s1,m')) ->
      let s = String.Set.diff s1 s0 in
      if String.Set.is_empty s then
        Format.fprintf ppf "@ @[<hv2>module %s : sig%a@;<1 -2>end@]"
          key (dump_map (String.Set.union s1 s0)) m'
      else
        Format.fprintf ppf "@ module %s = %s" key (String.Set.choose s))
    m

let process_ml_map =
  read_parse_and_extract Parse.implementation Depend.add_implementation_binding
                         String.Map.empty Pparse.Structure

let process_mli_map =
  read_parse_and_extract Parse.interface Depend.add_signature_binding
                         String.Map.empty Pparse.Signature

let process_mll_map fname =
  report_err (Failure (fname ^ " : maps are not supported on mll files."));
  String.Set.empty, String.Map.empty

let process_mly_map fname =
  report_err (Failure (fname ^ " : maps are not supported on mll files."));
  String.Set.empty, String.Map.empty

let parse_map fname =
  let old_transp = !Clflags.transparent_modules in
  Clflags.transparent_modules := true;
  let (deps, m) =
    process_file fname ~def:(String.Set.empty, String.Map.empty)
      ~ml_file:process_ml_map
      ~mli_file:process_mli_map
      ~mll_file:process_mll_map
      ~mly_file:process_mly_map
  in
  Clflags.transparent_modules := old_transp;
  let modname =
    String.capitalize_ascii Filename.(basename (chop_extension fname)) in
  if String.Map.is_empty m then
    report_err (Failure (fname ^ " : empty map file or parse error"));
  let mm = Depend.make_node m in
  if !debug then begin
    Format.printf "@[<v>%s:%t%a@]@." fname
      (fun ppf -> String.Set.iter (Format.fprintf ppf " %s") deps)
      (dump_map deps) (String.Map.add modname mm String.Map.empty)
  end;
  let mm = Depend.weaken_map (String.Set.singleton modname) mm in
  module_map := String.Map.add modname mm !module_map

(* Dependency processing *)

type dep_arg =
  | Map of Misc.filepath (* -map option *)
  (* -impl, -intf, -lex, -yacc, or anon arg *)
  | Src of Misc.filepath * file_kind option

let process_dep_arg = function
  | Map file -> parse_map file
  | Src (file, None) -> file_dependencies file
  | Src (file, (Some file_kind)) -> file_dependencies_as file_kind file

let process_dep_args dep_args = List.iter process_dep_arg dep_args

(* Entry point *)

let print_version () =
  Format.printf "ocamldep, version %s@." Sys.ocaml_version;
  exit 0

let print_version_num () =
  Format.printf "%s@." Sys.ocaml_version;
  exit 0


let run_main argv =
  let add_to_list l e = l := e :: !l in
  let dep_args_rev : dep_arg list ref = ref [] in
  let add_dep_arg f s = add_to_list dep_args_rev (f s) in
  Clflags.classic := false;
  try
    Compenv.readenv ppf Before_args;
    Clflags.reset_arguments (); (* reset arguments from ocamlc/ocamlopt *)
    Clflags.add_arguments __LOC__ [
      "-absname", Arg.Set Clflags.absname,
        " Show absolute filenames in error messages";
      "-no-absname", Arg.Clear Clflags.absname,
        " Do not try to show absolute filenames in error messages (default)";
      "-all", Arg.Set all_dependencies,
        " Generate dependencies on all files";
      "-allow-approx", Arg.Set allow_approximation,
        " Fallback to a lexer-based approximation on unparsable files";
      "-as-map", Arg.Set Clflags.transparent_modules,
        " Omit delayed dependencies for module aliases (-no-alias-deps -w -49)";
        (* "compiler uses -no-alias-deps, and no module is coerced"; *)
      "-debug-map", Arg.Set debug,
        " Dump the delayed dependency map for each map file";
      "-I", Arg.String (add_to_list Clflags.include_dirs),
        "<dir>  Add <dir> to the list of include directories";
      "-nocwd", Arg.Set nocwd,
        " Do not add current working directory to \
          the list of include directories";
      "-impl", Arg.String (add_dep_arg (fun f -> Src (f, Some ML))),
        "<f>  Process <f> as a .ml file";
      "-intf", Arg.String (add_dep_arg (fun f -> Src (f, Some MLI))),
        "<f>  Process <f> as a .mli file";
      "-lex", Arg.String (add_dep_arg (fun f -> Src (f, Some MLL))),
        "<f>  Process <f> as a .mll file";
      "-yacc", Arg.String (add_dep_arg (fun f -> Src (f, Some MLY))),
        "<f>  Process <f> as a .mly file";
      "-map", Arg.String (add_dep_arg (fun f -> Map f)),
        "<f>  Read <f> and propagate delayed dependencies to following files";
      "-ml-synonym", Arg.String(add_to_synonym_list ml_synonyms),
        "<e>  Consider <e> as a synonym of the .ml extension";
      "-mli-synonym", Arg.String(add_to_synonym_list mli_synonyms),
        "<e>  Consider <e> as a synonym of the .mli extension";
      "-mll-synonym", Arg.String(add_to_synonym_list mll_synonyms),
        "<e>  Consider <e> as a synonym of the .mll extension";
      "-mly-synonym", Arg.String(add_to_synonym_list mly_synonyms),
        "<e>  Consider <e> as a synonym of the .mly extension";
      "-modules", Arg.Set raw_dependencies,
        " Print module dependencies in raw form (not suitable for make)";
      "-native", Arg.Set native_only,
        " Generate dependencies for native-code only (no .cmo files)";
      "-bytecode", Arg.Set bytecode_only,
        " Generate dependencies for bytecode-code only (no .cmx files)";
      "-one-line", Arg.Set one_line,
        " Output one line per file, regardless of the length";
      "-open", Arg.String (add_to_list Clflags.open_modules),
        "<module>  Opens the module <module> before typing";
      "-plugin", Arg.String(fun _p -> Clflags.plugin := true),
        "<plugin>  (no longer supported)";
      "-pp", Arg.String(fun s -> Clflags.preprocessor := Some s),
        "<cmd>  Pipe sources through preprocessor <cmd>";
      "-ppx", Arg.String (add_to_list Compenv.first_ppx),
        "<cmd>  Pipe abstract syntax trees through preprocessor <cmd>";
      "-shared", Arg.Set shared,
        " Generate dependencies for native plugin files (.cmxs targets)";
      "-slash", Arg.Set Clflags.force_slash,
        " (Windows) Use forward slash / instead of backslash \\ in file paths";
      "-no-slash", Arg.Clear Clflags.force_slash,
        " (Windows) Preserve any backslash \\ in file paths";
      "-sort", Arg.Set sort_files,
        " Sort files according to their dependencies";
      "-version", Arg.Unit print_version,
        " Print version and exit";
      "-vnum", Arg.Unit print_version_num,
        " Print version number and exit";
      "-args", Arg.Expand Arg.read_arg,
        "<file> Read additional newline separated command line arguments \n\
        \      from <file>";
      "-args0", Arg.Expand Arg.read_arg0,
        "<file> Read additional NUL separated command line arguments from \n\
        \      <file>"
    ];
    let program = Filename.basename Sys.argv.(0) in
    Compenv.parse_arguments (ref argv)
      (add_dep_arg (fun f -> Src (f, None))) program;
    process_dep_args (List.rev !dep_args_rev);
    Compenv.readenv ppf Before_link;
    if !sort_files then sort_files_by_dependencies !files
    else List.iter print_file_dependencies (List.sort compare !files);
    (if Error_occurred.get () then 2 else 0)
  with
  | Compenv.Exit_with_status n ->
      n
  | exn ->
      Location.report_exception ppf exn;
      2


let main () =
  exit (run_main Sys.argv)

let main_from_option () =
  if Sys.argv.(1) <> "-depend" then begin
    Printf.eprintf
      "Fatal error: argument -depend must be used as first argument.\n%!";
    exit 2;
  end;
  let args =
    Array.concat [ [| Sys.argv.(0) ^ " -depend" |];
                   Array.sub Sys.argv 2 (Array.length Sys.argv - 2) ] in
  Sys.argv.(0) <- args.(0);
  exit (run_main args)
