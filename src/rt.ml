(* rt.ml *)
(*
 * This file is public domain as declared by Sturm Mabie
 *)

open Core
open Defs


module RArray = Res.Array
(*
 * Unfortunately, OCaml makes is unnecessarily difficult to create modules that
 * are mutually recursive. As such, for simplicity, we just throw all the
 * functions we need into this Rt module.
 *)
let stk = (RArray.empty () : xs_val RArray.t)

(* list index stack. used to quickly construct lists *)
let xstk: int Stack.t = Stack.create ()

(*
 * Every time we create a new handle, we bump this counter. 0, 1,2 are reserved
 * for stdin, stdout, and stderr, respectively.
 *)
let fdnum = ref 3

let rec is_oper ts k =
  match ts with
  | t :: ts ->
     (match Hashtbl.find t k with
      | Some F { is_oper = x; instrs = _ }  -> x
      | Some _ -> false
      | None -> is_oper ts k)
  | [] -> false

let convert idx = RArray.length stk - idx - 1
let get idx = RArray.get stk @ convert idx
let swap x y = RArray.swap stk (convert x) (convert y)
let len () = RArray.length stk
let push v = RArray.add_one stk v
let pop () = let v = get 0 in RArray.remove_one stk; v
let peek () = get 0
let dup () = push @ peek ()

let rec lookup ts k =
  match ts, k with
  | t :: ts, k ->
     (match Hashtbl.find t k with
      | Some x -> x
      | None -> lookup ts k)
  | [], _ -> failwith @ sprintf "%s not found" k

let pop_get ctxs =
  match pop () with
  | Q x -> lookup ctxs x
  | x -> x

let peek_get ctxs =
  match peek () with
  | Q x -> lookup ctxs x
  | x -> x

let create_builtin_fn ~is_oper  f = F { is_oper; instrs = Builtin f }
let create_fn ~is_oper xs = F { is_oper; instrs = User xs }

let create_ctx () = Hashtbl.create (module String)
let bind_ctx t (k : string) (v : xs_val) = Hashtbl.set t k v
let bind ts q v =
  match ts, q, v with
  | ctx :: _, Q k, v -> bind_ctx ctx k v
  | _ -> failwith "invalid types for bind"

let rec find_ctx ts (q : string) =
  match ts with
  | x :: xs ->
     if Option.is_some @ Hashtbl.find x q then Some x
     else find_ctx xs q
  | _ -> None

let setup xs =
  let ctx = create_ctx () in
  List.iter xs
    (function | s, is_oper, f -> bind_ctx ctx s @ create_builtin_fn is_oper f);
  ctx

let rec eval ctxs x =
  match x with
  | Ident x ->
     (match lookup ctxs x with
      | F _ as f -> call_fn f ctxs
      | x -> push x)
  | Expr xs ->
     let len = Array.length xs in
     let rec go idx =
       if idx >= len then ()
       else (
         match xs.(idx) with
         | Ident x as i when is_oper ctxs x ->
            (match xs.(idx + 1) with
             | Ident y   ->
                eval ctxs @ Quote y;
                eval ctxs i;
             | y ->
                eval ctxs y;
                eval ctxs i);
            go (idx + 2)
         | x -> eval ctxs x;
                go (idx + 1)
       ) in
     go 0
  | Fn xs -> push @ F { is_oper = false; instrs = User xs }
  | InfixFn xs -> push @ F { is_oper = true; instrs = User xs }
  | Int x -> push (Z x)
  | Float x -> push (R x)
  | Bool x -> push (B x)
  | Null -> push N
  | Str x -> push (S x)
  | Quote x -> push (Q x)
  | BoolList xs -> push @ L (Array.map xs (fun x -> B x))
  | _ -> ()

and  call_fn f ctxs =
  match f with
  | F { is_oper = _; instrs = Builtin f } -> f ctxs
  | F { is_oper = _; instrs = User xs } -> eval (create_ctx () :: ctxs) (Expr xs)
  | _ -> failwith "xs_val is not a function"

let pop_eval ctxs =
  match pop_get ctxs with
  | F _ as f -> pop @ call_fn f ctxs
  | x -> x

let display () =
  RArray.iteri
    (fun idx x -> printf "%d: %s\n" (len () - idx - 1) @ xs_to_string x)
    stk
