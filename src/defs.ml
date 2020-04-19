(* defs.ml *)
(* 
 * This file is public domain as declared by Sturm Mabie
 *)

open Core
open Res
open Stdio

let (@) = (@@)
let ($) = (@@)

let type_err s = raise @ Failure (Printf.sprintf "%s applied on invalid types" s)

type xs_val =
  | Z of int 
  | R of float 
  | Q of string 
  | B of bool 
  | S of string 
  | F of fn_t 
  | L of xs_val Array.t
  | N 
and parse_val =
  | Sep 
  | Int of int 
  | Float of float 
  | Quote of string 
  | Ident of string 
  | Bool of bool 
  | Null 
  | Str of string 
  | Fn of parse_val list 
  | InfixFn of parse_val list 
  | Expr of parse_val list 
and fn_t =
  { is_oper: bool
  ; instrs: (parse_val list, (string, xs_val) Hashtbl.t list -> unit) Either.t
  }

let rec concat_parse xs = String.concat ~sep:" " @ List.rev_map xs parse_print
and parse_print x =
  match x with
  | Int x -> sprintf "%d" x
  | Expr xs -> concat_parse xs
  | Quote x -> sprintf "`%s" x
  | Null -> "0N"
  | Fn xs -> sprintf "(%s)" @ concat_parse xs
  | InfixFn xs -> sprintf "{%s}" @ concat_parse xs
  | Str x -> sprintf "`%s" x
  | Ident x -> sprintf "%s" x
  | Bool x -> sprintf "%b" x
  | Float x -> sprintf "%f" x
  | Sep -> ""
and xs_print x =
  match x with
  | Z x -> sprintf "%d" x
  | R x -> sprintf "%f" x
  | Q x -> sprintf "`%s" x
  | B x -> sprintf "%b" x
  | S x -> sprintf "\"%s\"" x
  | L xs ->
     sprintf "[%s]" @
       String.concat ~sep:" " @ List.map (Array.to_list xs) xs_print
  | F { is_oper = b; instrs = Either.First xs } ->
     sprintf (if b == true then "{%s}" else "(%s)") @ concat_parse xs
  | N -> "0N"
  | _ -> ""

(* append a list to an array  *)
let res_append t xs = List.iter xs (fun x -> Array.add_one t x)
