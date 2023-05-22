(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Antonin Decimo, Tarides                         *)
(*                                                                        *)
(*                         Copyright 2023 Tarides                         *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* A small lexer for .mly (ocamlyacc) files, extracting OCaml code
   fragments: the header, semantic actions, and the trailer. *)

{
let brace_depth = ref 0

exception Lexical_error of string * string * int * int

(* for debugging


}
*)
let string_of_pos loc =
  let { Lexing.pos_fname; pos_lnum; pos_bol; pos_cnum } = loc in
  Printf.sprintf "{ fname: %S; lnum: %d; bol: %d; cnum: %d } %d"
    pos_fname pos_lnum pos_bol pos_cnum (pos_cnum - pos_bol)

type token =
  | Theader of Lexing.position * Lexing.position
  | Taction of Lexing.position * Lexing.position
  | Ttrailer of Lexing.position * Lexing.position
  | Teof

type state = HeaderDeclarations | Rules | Trailer
let state = ref HeaderDeclarations
}

rule main = parse
  | "/*"
    { comment_yacc lexbuf;
      main lexbuf }
  | '\010'
    { Lexing.new_line lexbuf;
      main lexbuf }
  | "%{"
    { let start = Lexing.lexeme_end_p lexbuf in
      Theader (start, header lexbuf) }
  | "%%"
    { match !state with
      | HeaderDeclarations ->
         state := Rules;
         rules lexbuf
      | Rules ->
         state := Trailer;
         let start = Lexing.lexeme_end_p lexbuf in
         Ttrailer (start, trailer lexbuf)
      | Trailer -> assert false }
  | "{"
    { incr brace_depth;
      match !state with
      | Rules ->
         let start = Lexing.lexeme_end_p lexbuf in
         Taction (start, action lexbuf)
      | HeaderDeclarations | Trailer -> main lexbuf }
  | eof { Teof }
  | _ { main lexbuf }

and header = parse
  | "(*"   { comment_ocaml lexbuf; header lexbuf }
  | '\010' { Lexing.new_line lexbuf; header lexbuf }
  | "%}"   { Lexing.lexeme_start_p lexbuf }
  | eof    { raise (Lexical_error ("unterminated header", "", 0, 0)) }
  | _      { header lexbuf }

and rules = parse
  | "/*"
    { comment_yacc lexbuf; rules lexbuf }
  | "{"
    { incr brace_depth;
      let start = Lexing.lexeme_end_p lexbuf in
      Taction (start, action lexbuf) }
  | '\010'
    { Lexing.new_line lexbuf; rules lexbuf }
  | "%%"
    { assert (!state = Rules);
      state := Trailer;
      let start = Lexing.lexeme_end_p lexbuf in
      Ttrailer (start, trailer lexbuf) }
  | eof { Teof }
  | _ { rules lexbuf }

and action = parse
  | "(*"   { comment_ocaml lexbuf; action lexbuf }
  | "{"    { incr brace_depth; action lexbuf }
  | "}"    { decr brace_depth;
             if !brace_depth = 0 then Lexing.lexeme_start_p lexbuf
             else action lexbuf }
  | '\010' { Lexing.new_line lexbuf; action lexbuf }
  | eof    { raise (Lexical_error ("unterminated action", "", 0, 0)) }
  | _      { action lexbuf }

and trailer = parse
  | "(*"   { comment_ocaml lexbuf; trailer lexbuf }
  | "{"    { incr brace_depth; trailer lexbuf }
  | "}"    { decr brace_depth;
             if !brace_depth = 0 then Lexing.lexeme_start_p lexbuf
             else trailer lexbuf }
  | eof    { Lexing.lexeme_start_p lexbuf }
  | '\010' { Lexing.new_line lexbuf; trailer lexbuf }
  | _      { trailer lexbuf }

and comment_ocaml = parse
  | "*)"   { () }
  | eof    { raise (Lexical_error ("unterminated comment", "", 0, 0)) }
  | '\010' { Lexing.new_line lexbuf; comment_ocaml lexbuf }
  | _      { comment_ocaml lexbuf }

and comment_yacc = parse
  | "*/"   { () }
  | eof    { raise (Lexical_error ("unterminated comment", "", 0, 0)) }
  | '\010' { Lexing.new_line lexbuf; comment_yacc lexbuf }
  | _      { comment_yacc lexbuf }
