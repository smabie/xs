open Core
open Res

open Defs

(* 
 * Unfortunately, OCaml makes is unnecessarily difficult to create modules that
 * are mutually recursive. As such, for simplicity, we just throw all the 
 * functions we need into this Rt module.
 *)
let stk = (Array.empty () : xs_val Array.t)

let rec is_oper ts k =
  match ts with
  | t::ts ->
     (match Hashtbl.find t k with
      | Some F { is_oper = x; instrs = _ }  -> x
      | Some _ -> false
      | None -> is_oper ts k)
  | [] -> false

let convert idx = Array.length stk - idx - 1
let get idx = stk.(convert idx)
let swap x y = Array.swap stk (convert x) (convert y)
let len () = Array.length stk
let push v = Array.add_one stk v
let pop () = let v = get 0 in Array.remove_one stk; v
let peek () = get 0

let rec lookup ts k =
  match ts, k with
  | t::ts, k ->
     (match Hashtbl.find t k with
      | Some x -> x
      | None -> lookup ts k)
  | [], _ -> raise @ Failure (sprintf "%s not found" k)

let pop_get ctxs =
  match pop () with
  | Q x -> lookup ctxs x
  | x -> x

let peek_get ctxs =
  match peek () with
  | Q x -> lookup ctxs x
  | x -> x 

let create_builtin_fn ~is_oper  f = F { is_oper; instrs = Either.Second f }
let create_fn ~is_oper xs = F { is_oper; instrs = Either.First xs }

let create_ctx () = Hashtbl.create (module String)
let bind_ctx t (k : string) (v : xs_val) = Hashtbl.set t k v
let setup xs =
  let ctx = create_ctx () in
  List.iter xs
    (function | s, is_oper, f -> bind_ctx ctx s @ create_builtin_fn is_oper f);
  ctx

let rec eval ctxs x =
  let swap_opers ctxs xs =
    let rec go idx =
      if idx >= Array.length xs then xs
      else
        match xs.(idx) with
        | Ident x when is_oper ctxs x ->
           Array.swap xs idx (idx + 1);
           (match xs.(idx) with
            | Ident x -> xs.(idx) <- Quote x
            | _ -> ());
           go (idx + 2)
        | _ -> go (idx + 1) in
    go 0 in
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
     (match lookup ctxs x with
      | F { is_oper = _; instrs = _ } as f -> call_fn f (create_ctx () :: ctxs)
      | x -> push x)
  | Fn xs -> push @ F { is_oper = false; instrs = Either.First xs }
  | InfixFn xs -> push @ F { is_oper = true; instrs = Either.First xs }
  | _ -> ()

and  call_fn f ctxs =
  match f with
  | F { is_oper = _; instrs = Either.Second f } -> f ctxs
  | F { is_oper = _; instrs = Either.First xs } -> eval ctxs (Expr xs)
  | _ -> raise @ Failure "xs_val is not a function"

let pop_eval ctxs =
  match pop_get ctxs with
  | F { is_oper = _; instrs = Either.Second f } -> pop @ f ctxs
  | F { is_oper = _; instrs = Either.First xs } -> pop @ eval ctxs @ Expr xs
  | x -> x

let display () =
  Array.iteri (fun idx x -> printf "%d: %s\n" idx @ xs_print x) stk
