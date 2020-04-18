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
