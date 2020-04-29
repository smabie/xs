(* defs.ml *)
(*
 * This file is public domain as declared by Sturm Mabie
 *)

open Core

let (@) = (@@)

let type_err s = failwith @ Printf.sprintf "%s applied on invalid types" s


type xs_val =
  | Z of int                    (* int *)
  | R of float                  (* real *)
  | Q of string                 (* quote *)
  | B of bool                   (* bool *)
  | S of string                 (* string *)
  | F of fn_t                   (* function *)
  | L of xs_val Array.t         (* list *)
  | N                           (* null *)
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

let rec concat_parse xs =
  String.concat ~sep:" " @ List.rev_map xs parse_to_string
and parse_to_string x =
  match x with
  | Int x -> sprintf "%d" x
  | Expr xs -> concat_parse xs
  | Quote x -> sprintf "`%s" x
  | Null -> "0N"
  | Fn xs -> sprintf "(%s)" @ concat_parse xs
  | InfixFn xs -> sprintf "{%s}" @ concat_parse xs
  | Str x -> sprintf "`%s" x
  | Ident x -> sprintf "%s" x
  | Bool true -> "1b"
  | Bool false -> "0b"
  | Float x -> sprintf "%f" x
  | Sep -> ""
and xs_to_string x =
  match x with
  | Z x -> sprintf "%d" x
  | R x -> sprintf "%f" x
  | Q x -> sprintf "`%s" x
  | B true -> "1b"
  | B false -> "0b"
  | S x -> sprintf "\"%s\"" x
  | L xs ->
     sprintf "[%s]" @
       String.concat ~sep:" " @ List.map (Array.to_list xs) xs_to_string
  | F { is_oper = b; instrs = Either.First xs } ->
     sprintf (if phys_equal b true then "{%s}" else "(%s)") @ concat_parse xs
  | N -> "0N"
  | _ -> ""

let rec xs_eq x y =
  match x, y with
  | N, N -> true
  | Z x, Z y -> x = y
  | R x, R y -> Float.(abs (x - y) <= 10. ** (-5.))
  | B x, B y -> Bool.equal x y
  | S x, S y -> String.equal x y
  | Q x, Q y -> String.equal x y
  | (F _ as fx), (F _ as fy) ->
     String.equal (xs_to_string fx) (xs_to_string fy)
  | L xs, L ys when Array.length xs = Array.length ys ->
     let len = Array.length xs in
     let rec go idx =
       if idx = len then true
       else if xs_eq xs.(idx) ys.(idx) then go (idx + 1)
       else false in
     go 0
  | _ -> false
