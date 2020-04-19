(* stack.re *)
(* 
 * This file is public domain as declared by Sturm Mabie
 *)

open Core
open Res
open Context

open Defs

let stk = (Array.empty () : xs_val Array.t)

let convert idx = Array.length stk - idx - 1

let get idx = stk.(convert idx)
let swap x y = Array.swap stk (convert x) (convert y)
let len () = Array.length stk

let push v = Array.add_one stk v
let pop () = let v = get 0 in Array.remove_one stk; v
let peek () = get 0
let pop_get ctxs =
  match pop () with
  | Q x -> Ctx.lookup ctxs x
  | x -> x

let display () =
  Array.iteri (fun idx x -> printf "%d: %s\n" idx @ xs_print x) stk  

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
  | Int x -> push (Z x)
  | Float x -> push (R x)
  | Bool x -> push (B x)
  | Null -> push N
  | Str x -> push (S x)
  | Quote x -> push (Q x)
  | Expr xs ->
     Array.of_list xs |> swap_opers ctxs |> Array.iter (eval ctxs)
  | Ident x ->
     (match Ctx.lookup ctxs x with
      | F { is_oper = _; instrs = Either.Second f } -> f ctxs
      | F { is_oper = _; instrs = Either.First xs } -> ()
      | x -> push x)
  | Fn xs -> push @ F { is_oper = false; instrs = Either.First xs }
  | InfixFn xs -> push @ F { is_oper = true; instrs = Either.First xs }
  | _ -> ()

let pop_eval ctxs =
  match pop_get ctxs with
  | F { is_oper = _; instrs = Either.Second f } -> f ctxs
  | F { is_oper = _; instrs = Either.First xs } -> eval ctxs @ Expr xs
  | _ -> ()
