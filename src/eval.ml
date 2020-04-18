
open Core
open Res

open Defs
open Context

let swap_opers ctxs xs =
  let rec go idx =
    if idx >= Array.length xs then xs
    else
      match xs.(idx) with
      | Ident x when Ctx.is_oper ctxs x ->
         Array.swap xs idx (idx + 1);
         (match xs.(idx) with
          | Ident x -> xs.(idx) <- Quote x
          | _ -> ());
         go (idx + 2)
      | _ -> go (idx + 1) in
  go 0

let rec eval ctxs x =
  match x with
  | Int x -> Stk.push (Z x)
  | Float x -> Stk.push (R x)
  | Bool x -> Stk.push (B x)
  | Null -> Stk.push N
  | Str x -> Stk.push (S x)
  | Quote x -> Stk.push (Q x)
  | Expr xs ->
     Array.of_list xs |> swap_opers ctxs |> Array.iter (eval ctxs)
  | Ident x ->
     (match Ctx.lookup ctxs x with
      | F { is_oper = _; instrs = Either.Second f } -> f ctxs
      | F { is_oper = _; instrs = Either.First xs } -> ()
      | x -> Stk.push x)
  | _ -> ()
