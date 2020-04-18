open Context
open Core
open Res
open Defs

let rec with_list ctxs f = list_end ctxs; f (); list_begin ctxs
and add ctxs =
  let x = Stk.pop_get ctxs in
  let y = Stk.pop () in
  
    match x, y with
    | Z x, Z y  -> Stk.push @ Z (x + y)
    | Z x, R y  -> Stk.push @ R (float_of_int x +. y)
    | R x, Z y  -> Stk.push @ R (x +. float_of_int y)
    | R x, R y  -> Stk.push @ R (x +. y)
    | L xs, L ys ->
       if Array.length xs <> Array.length ys then
         raise $ Failure "+: list length unequal"
       else
         with_list ctxs @
           fun () ->
           let len = Array.length xs in
           let rec go idx =
             if idx == len then ()
             else (
               Stk.push @ Array.get xs idx;
               Stk.push @ Array.get ys idx;
               add ctxs; go (idx + 1)
             ) in go 0
    | x, L ys ->
       with_list ctxs @
         fun () -> Array.iter (fun y -> Stk.push y; Stk.push x; add ctxs) ys;
    | _ -> type_err "+"

and sub ctxs = 
  let x = Stk.pop_get ctxs in
  let y = Stk.pop () in
  Stk.push $
    match x, y with
    | Z x, Z y -> Z (x - y)
    | Z x, R y -> R (float_of_int x -. y)
    | R x, Z y -> R (x -. float_of_int y)
    | R x, R y -> R (x -. y)
    | _ -> type_err "-"

and mul ctxs =
  let x = Stk.pop_get ctxs in
  let y = Stk.pop () in
  Stk.push $
    match x, y with
    | Z x, Z y -> Z (x * y)
    | Z x, R y -> R (float_of_int x *. y)
    | R x, Z y -> R (x *. float_of_int y)
    | R x, R y -> R (x *. y)
    | _ -> type_err "*"

and div ctxs =
  let x = Stk.pop_get ctxs in
  let y = Stk.pop () in
  Stk.push $
    match x, y with
    | Z x, Z y -> Z (x / y)
    | Z x, R y -> R (float_of_int x /. y)
    | R x, Z y -> R (x /. float_of_int y)
    | R x, R y -> R (x /. y)
    | _ -> type_err "%"

and neg ctxs =
  Stk.push $
    match Stk.pop () with
    | Z x -> Z (-1 * x)
    | R x -> R (-1.0 *. x)
    | _ -> type_err "neg"

and set ctxs =
  let q = Stk.pop () in
  let v = Stk.pop () in
  match q, v with
  | Q x, y ->
     (match List.hd ctxs with
      | Some ctx -> Ctx.bind ctx x y
      | None -> raise $ Failure "no context found")
  | _ -> type_err ":"

and apply ctxs =
  match Stk.pop_get ctxs with
  | F { is_oper = _; instrs = Either.Second f } -> f ctxs
  | F { is_oper = _; instrs = Either.First xs } -> ()
  | _ -> type_err "."

and list_end ctxs = Stk.push N
and list_begin ctxs =
  let xs = Array.empty () in
  let rec go () =
    match Stk.pop () with
    | N -> Stk.push $ L xs
    | x -> (Array.add_one xs x; go ()) in
  go ()

and fold ctxs =
  let f = Stk.pop_get ctxs in
  let x = Stk.pop () in
  match (f, x) with
  | F { is_oper = _; instrs = Either.Second f }, L xs ->
     let fn b a =
       (match b, a with
        | N, y -> y
        | x, y -> Stk.push y; Stk.push x; f ctxs; Stk.pop ()) in
     Stk.push $ Array.fold_left fn N xs
  | F { is_oper = _; instrs = Either.First ys }, L xs -> ()
  | _ -> type_err "/"


let builtin =
  [("+",        true,   add);
   ("-",        true,   sub);
   ("*",        true,   mul);
   ("%",        true,   div);
   ("neg",      false,  neg);
   (":",        true,   set);
   (".",        true,   apply);
   ("]",        false,  list_end);
   ("[",        false,  list_begin);
   ("/",        true,   fold)]

