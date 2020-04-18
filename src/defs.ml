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
  ; instrs: (xs_val Array.t, (string, xs_val) Hashtbl.t list -> unit) Either.t
  }

let rec parse_print x =
  match x with
  | Int x -> printf "%d " x
  | Expr xs -> List.iter xs parse_print
  | Quote x -> printf "`%s " x
  | Null -> printf "0N "
  | Fn xs -> List.iter xs parse_print
  | InfixFn xs -> List.iter xs parse_print
  | Str x -> printf "`%s " x
  | Ident x -> printf "%s " x
  | Bool x -> printf "%b " x
  | Float x -> printf "%f " x
  | Sep -> ()

let rec xs_print x =
  match x with
  | Z x -> sprintf "%d" x
  | R x -> sprintf "%f" x
  | Q x -> sprintf "`%s" x
  | B x -> sprintf "%b" x
  | N -> Printf.sprintf "0N"
  | S x -> sprintf "\"%s\"" x
  | L xs ->
     sprintf "[%s]"
       (String.concat ~sep:" " @ List.map (Array.to_list xs) xs_print)
  | _ -> ""


(* append a list to an array  *)
let res_append t xs = List.iter xs (fun x -> Array.add_one t x)

