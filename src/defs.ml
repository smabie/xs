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
  | L of xs_val array           (* list *)
  | H of (int * handle_kind)
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
  | Fn of parse_val array
  | InfixFn of parse_val array
  | Expr of parse_val array
and fn_t =
  { is_oper: bool
  ; instrs: fn_kind
  }
and fn_kind =
  | Builtin of ((string, xs_val) Hashtbl.t list -> unit)
  | User of parse_val array
and handle_kind =
  | In of In_channel.t
  | Out of Out_channel.t

let rec concat_parse xs =
  String.concat ~sep:" " @ List.rev_map ~f:parse_to_string @ Array.to_list xs
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
  | R x -> sprintf "%.5f" x
  | Q x -> sprintf "`%s" x
  | B true -> "1b"
  | B false -> "0b"
  | S x -> sprintf "\"%s\"" x
  | L [||] -> "[]"
  | L xs ->
     Array.fold_until xs ~init:(Buffer.create (Array.length xs + 1))
       ~finish:(fun acc -> Buffer.add_char acc 'b'; Buffer.contents acc)
       ~f:(fun acc y ->
         match y with
         | B x ->
            Buffer.add_char acc (if x then '1' else '0');
            Continue (acc)
         | _ ->
            Stop (sprintf "[%s]" @
                    String.concat ~sep:" " @
                      List.map (Array.to_list xs) xs_to_string))
  | F { is_oper = b; instrs = User xs } ->
     sprintf (if phys_equal b true then "{%s}" else "(%s)") @ concat_parse xs
  | H (x, _) -> sprintf "`H%d" x
  | N -> "0N"
  | _ -> ""

module Xs = struct
  let tolerance = Float.(10. ** -5.)
  module T = struct
    type t = xs_val
    let rec compare x y =
      let fcomp x y =
        Float.(
          if between (abs @ x - y) 0. tolerance then 0
          else if x < y then -1
          else 1
        ) in
      match x, y with
      | N, N -> 0
      | N, L _ -> 1
      | N, _ -> -1
      | Z x, Z y -> Int.compare x y
      | Z x, R y -> fcomp (Int.to_float x) y
      | Z x, B y -> Int.compare x (Bool.to_int y)
      | R x, R y -> fcomp x y
      | R x, Z y -> fcomp x (Int.to_float y)
      | R x, B y -> fcomp x (Int.to_float @ Bool.to_int y)
      | S x, S y | Q x, Q y -> String.compare x y
      | B x, B y -> Bool.compare x y
      | L [||], L [||] -> 0
      | S _, _ -> 1
      | Q _, _ -> 1
      | _, S _ -> -1
      | _, Q _ -> -1
      | _, L [||] -> 1
      | L [||], _ -> -1
      | L xs, L ys -> Array.compare compare xs ys
      | (F _ as fx), (F _ as fy) ->
         String.compare (xs_to_string fx) (xs_to_string fy)
      | _, L _ -> -1
      | _ -> 1
    let equal x y = compare x y = 0
    let sexp_of_t t = Sexp.of_string @ xs_to_string t
    let to_string = xs_to_string
  end

  include T
  include Comparator.Make(T)
end
