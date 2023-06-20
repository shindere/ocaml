val brace_depth : int ref
exception Lexical_error of string * string * int * int
val string_of_pos : Lexing.position -> string
type token =
    Theader of Lexing.position * Lexing.position
  | Taction of Lexing.position * Lexing.position
  | Ttrailer of Lexing.position * Lexing.position
  | Teof
type state = HeaderDeclarations | Rules | Trailer
val state : state ref
val __ocaml_lex_tables : Lexing.lex_tables
val main : Lexing.lexbuf -> token
val __ocaml_lex_main_rec : Lexing.lexbuf -> int -> token
val header : Lexing.lexbuf -> Lexing.position
val __ocaml_lex_header_rec : Lexing.lexbuf -> int -> Lexing.position
val rules : Lexing.lexbuf -> token
val __ocaml_lex_rules_rec : Lexing.lexbuf -> int -> token
val action : Lexing.lexbuf -> Lexing.position
val __ocaml_lex_action_rec : Lexing.lexbuf -> int -> Lexing.position
val trailer : Lexing.lexbuf -> Lexing.position
val __ocaml_lex_trailer_rec : Lexing.lexbuf -> int -> Lexing.position
val comment_ocaml : Lexing.lexbuf -> unit
val __ocaml_lex_comment_ocaml_rec : Lexing.lexbuf -> int -> unit
val comment_yacc : Lexing.lexbuf -> unit
val __ocaml_lex_comment_yacc_rec : Lexing.lexbuf -> int -> unit
