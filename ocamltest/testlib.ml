(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sébastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Miscellaneous library functions *)

let is_blank c =
  c = ' ' || c = '\012' || c = '\n' || c = '\r' || c =  '\t'

let words s =
  let l = String.length s in
  let i = ref (l-1) and j = ref (l-1) in
  let find_word () =
    while !j >= 0 && (is_blank s.[!j]) do decr j; done;
    if !j>=0 then begin
      i := !j - 1;
      while !i >= 0 && (not (is_blank s.[!i])) do decr i; done;
      let word = String.sub s (!i+1) (!j - !i) in
      j := !i - 1;
      Some word
    end else None in
  let rec f words_acc = match find_word() with
    | None -> words_acc
    | Some word -> f (word :: words_acc) in
  f []


let file_is_empty filename =
  let ic = open_in filename in
  let filesize = in_channel_length ic in
  close_in ic;
  filesize = 0

let string_of_location loc =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  Location.print_loc fmt loc;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let run_system_command command = match Sys.command command with
  | 0 -> ()
  | _ as exitcode ->
    Printf.eprintf "Sysem command %s failed with status %d\n%!"
      command exitcode;
    exit 3

let mkdir dir =
  if not (Sys.file_exists dir) then run_system_command ("mkdir " ^ dir)

let rec make_directory dir =
  if Sys.file_exists dir then ()  
  else (make_directory (Filename.dirname dir); mkdir dir)

let string_of_file filename =
  let chan = open_in filename in
  let filesize = in_channel_length chan in
  if filesize > Sys.max_string_length then
  begin
    close_in chan;
    filename
  end else begin
    let result = really_input_string chan filesize in
    close_in chan;
    result
  end

let with_input_file ?(bin=false) x f =
  let ic = (if bin then open_in_bin else open_in) x in
  try let res = f ic in close_in ic; res with e -> (close_in ic; raise e)

let with_output_file ?(bin=false) x f =
  let oc = (if bin then open_out_bin else open_out) x in
  try let res = f oc in close_out oc; res with e -> (close_out oc; raise e)


let copy_chan ic oc =
  let m = in_channel_length ic in
  let m = (m lsr 12) lsl 12 in
  let m = max 16384 (min Sys.max_string_length m) in
  let buf = Bytes.create m in
  let rec loop () =
    let len = input ic buf 0 m in
    if len > 0 then begin
      output oc buf 0 len;
      loop ()
    end
  in loop ()

let copy_file src dest =
  with_input_file ~bin:true src begin fun ic ->
    with_output_file ~bin:true dest begin fun oc ->
      copy_chan ic oc
    end
  end

