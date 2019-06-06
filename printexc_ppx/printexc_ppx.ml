module M = Ast_mapper
module H = Ast_helper
open Asttypes
open Parsetree
open Longident
open Format

let super = M.default_mapper

let extract_name arg =
  match arg.pexp_desc with
  | Pexp_construct (name, _) -> Format.asprintf "%a" Printtyp.longident name.txt; 
  | _ -> ""

let get_line_num arg =
  match arg.pexp_desc with
  | Pexp_construct (name, _) -> Printf.sprintf "%d" name.loc.loc_start.pos_lnum; 
  | _ -> ""

let is_raise_id expr =
  match expr.pexp_desc with
  | Pexp_ident {txt = Lident "raise"; _} -> true
  | _ -> false

let option_is_raise expr =
  match expr.pexp_desc with
  | Pexp_apply (fn, [_, constr]) -> if is_raise_id fn 
      then Some ((extract_name constr) ^ " on line " ^ (get_line_num constr)) 
      else None
  | _ -> None

let create_raise_string exc_name =
   H.Exp.constant @@
   H.Const.string ("raised " ^ exc_name) 

let create_print str loc = H.Exp.apply 
      ( H.Exp.ident {txt = Lident "print_endline"; loc})
      ([Nolabel, str])

let e_mapper mapper expr =
    match option_is_raise expr with
    | Some name -> H.Exp.sequence (create_print (create_raise_string name) expr.pexp_loc) expr
    | None -> super.M.expr mapper expr

let () = M.register "exception printer" (fun _ ->
    { super with M.expr = e_mapper }
  )