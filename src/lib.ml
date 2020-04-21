open Core
open Res
open Defs

let rec with_list ctxs f = list_end ctxs; f (); list_begin ctxs

and map2 ctxs xs ys f =
  if Array.length xs <> Array.length ys then
    raise @ Failure "list length unequal"
  else
    with_list ctxs @
      fun () ->
      let rec go idx =
        if idx = -1 then ()
        else (
          Rt.push @ Array.get xs idx;
          Rt.push @ Array.get ys idx;
          f ctxs;
          go (idx - 1)
        ) in
      go @ Array.length xs  - 1

and broadcast ctxs x ys f =
  with_list ctxs @
    fun () ->
    let rec go idx =
      if idx = -1 then ()
      else (
        Rt.push @ Array.get ys idx; Rt.push x;
        f ctxs; go (idx - 1)
      ) in go @ Array.length ys - 1

and add ctxs =                  (* + *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop_eval ctxs in
  match x, y with
  | Z x, Z y  -> Rt.push @ Z (x + y)
  | Z x, R y  -> Rt.push @ R (float_of_int x +. y)
  | R x, Z y  -> Rt.push @ R (x +. float_of_int y)
  | R x, R y  -> Rt.push @ R (x +. y)
  | L xs, L ys -> map2 ctxs xs ys add
  | x, L ys | L ys, x -> broadcast ctxs x ys add
  | _ -> type_err "+"

and sub ctxs =                  (* - *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop_eval ctxs in
  match x, y with
  | Z x, Z y -> Rt.push @ Z (x - y)
  | Z x, R y -> Rt.push @ R (float_of_int x -. y)
  | R x, Z y -> Rt.push @ R (x -. float_of_int y)
  | R x, R y -> Rt.push @ R (x -. y)
  | L xs, L ys -> map2 ctxs xs ys sub
  | x, L ys | L ys, x -> broadcast ctxs x ys sub
  | _ -> type_err "-"

and mul ctxs =                  (* * *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop_eval ctxs in
  
  match x, y with
  | Z x, Z y -> Rt.push @ Z (x * y)
  | Z x, R y -> Rt.push @ R (float_of_int x *. y)
  | R x, Z y -> Rt.push @ R (x *. float_of_int y)
  | R x, R y -> Rt.push @ R (x *. y)
  | L xs, L ys -> map2 ctxs xs ys mul
  | x, L ys | L ys, x -> broadcast ctxs x ys mul
  | _ -> type_err "*"

and div ctxs =                  (* % *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop_eval ctxs in
  match x, y with
  | Z x, Z y -> Rt.push @ Z (x / y)
  | Z x, R y -> Rt.push @ R (float_of_int x /. y)
  | R x, Z y -> Rt.push @ R (x /. float_of_int y)
  | R x, R y -> Rt.push @ R (x /. y)
  | L xs, L ys -> map2 ctxs xs ys div
  | x, L ys | L ys, x -> broadcast ctxs x ys div
  | _ -> type_err "%"

and neg ctxs =                  (* neg *)
  Rt.push @
    match Rt.pop_eval ctxs with
    | Z x -> Z (-1 * x)
    | R x -> R (-1.0 *. x)
    | _ -> type_err "neg"

and set ctxs =                  (* : *)
  let q = Rt.pop () in
  let v = Rt.pop_get ctxs in
  match q, v with
  | Q x, y ->
     (match List.hd ctxs with
      | Some ctx -> Rt.bind_ctx ctx x y
      | None -> raise @ Failure "no context found")
  | _ -> type_err ":"

and apply ctxs =                (* . *)
  match Rt.pop_get ctxs with
  | F { is_oper = _; instrs = _ } as f -> Rt.call_fn f ctxs
  | _ -> type_err "."

and list_end ctxs = Rt.push N   (* ] *)
and list_begin ctxs =           (* [ *)
  let xs = Array.empty () in
  let rec go () =
    match Rt.pop () with
    | N -> Rt.push @ L xs
    | x -> Array.add_one xs x; go () in
  go ()

and fold ctxs =                 (* / *)
  let f = Rt.pop_get ctxs in
  let x = Rt.pop_eval ctxs in
  match f, x with
  | F { is_oper = _; instrs = _} as f, L xs ->
     let fn b a =
       (match b, a with
        | N, y -> y
        | x, y -> Rt.push y; Rt.push x; Rt.call_fn f ctxs; Rt.pop ()) in
     Rt.push @ Array.fold_left fn N xs
  | _ -> type_err "/"

and map ctxs =                  (* ' *)
  let f = Rt.pop_get ctxs in
  let x = Rt.pop_eval ctxs in
  match f, x with
  | F { is_oper = _; instrs = _} as f, L xs ->
     with_list ctxs @ 
       fun () ->
       let rec go idx =
         if idx == -1 then ()
         else (
           Rt.push @ Array.get xs idx;
           Rt.call_fn f ctxs;
           go (idx - 1)
         ) in
       go @ Array.length xs - 1
  | _ -> type_err "'"
  
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
   ("/",        true,   fold);
   ("'",        true,   map)]
