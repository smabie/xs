open Core
open Res
open Defs

let rec with_list ctxs f = op_list_end ctxs; f (); op_list_begin ctxs

and map2 ctxs xs ys f =
  if Array.length xs <> Array.length ys then
    raise @ Failure "list length unequal"
  else
    with_list ctxs @
      fun () ->
      let rec go idx =
        if idx = -1 then ()
        else (
          Rt.push ys.(idx);
          Rt.push xs.(idx);
          f ctxs;
          go (idx - 1)
        ) in
      go @ Array.length xs - 1

and broadcast  ~rev ctxs x ys f =
  with_list ctxs @
    fun () ->
    let rec go idx =
      if idx = -1 then ()
      else (
        if rev then (
          Rt.push x; Rt.push ys.(idx)
        ) else (
          Rt.push ys.(idx); Rt.push x; 
        );
        f ctxs; go (idx - 1)
      ) in go @ Array.length ys - 1

and op_add ctxs =               (* + *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop_get ctxs in
  match x, y with
  | Z x, Z y  -> Rt.push @ Z (x + y)
  | Z x, R y  -> Rt.push @ R (float_of_int x +. y)
  | R x, Z y  -> Rt.push @ R (x +. float_of_int y)
  | R x, R y  -> Rt.push @ R (x +. y)
  | L xs, L ys -> map2 ctxs xs ys op_add
  | x, L ys -> broadcast ~rev:false ctxs x ys op_add
  | L ys, x -> broadcast ~rev:true ctxs x ys op_add
  | _ ->
     type_err "+"

and op_sub ctxs =               (* - *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop_get ctxs in
  match x, y with
  | Z x, Z y -> Rt.push @ Z (x - y)
  | Z x, R y -> Rt.push @ R (float_of_int x -. y)
  | R x, Z y -> Rt.push @ R (x -. float_of_int y)
  | R x, R y -> Rt.push @ R (x -. y)
  | L xs, L ys -> map2 ctxs xs ys op_sub
  | x, L ys -> broadcast ~rev:false ctxs x ys op_sub
  | L ys, x -> broadcast ~rev:true ctxs x ys op_sub
  | _ -> type_err "-"

and op_mul ctxs =               (* * *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop_get ctxs in
  match x, y with
  | Z x, Z y -> Rt.push @ Z (x * y)
  | Z x, R y -> Rt.push @ R (float_of_int x *. y)
  | R x, Z y -> Rt.push @ R (x *. float_of_int y)
  | R x, R y -> Rt.push @ R (x *. y)
  | L xs, L ys -> map2 ctxs xs ys op_mul
  | x, L ys -> broadcast ~rev:false ctxs x ys op_mul
  | L ys, x -> broadcast ~rev:true ctxs x ys op_mul
  | _ -> type_err "*"

and op_div ctxs =               (* % *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop_get ctxs in
  match x, y with
  | Z x, Z y -> Rt.push @ Z (x / y)
  | Z x, R y -> Rt.push @ R (float_of_int x /. y)
  | R x, Z y -> Rt.push @ R (x /. float_of_int y)
  | R x, R y -> Rt.push @ R (x /. y)
  | L xs, L ys -> map2 ctxs xs ys op_div
  | x, L ys -> broadcast ~rev:false ctxs x ys op_div
  | L ys, x -> broadcast ~rev:true ctxs x ys op_div
  | _ -> type_err "%"

and op_neg ctxs =               (* neg *)
  Rt.push @
    match Rt.pop_get ctxs with
    | Z x -> Z (-1 * x)
    | R x -> R (-1.0 *. x)
    | _ -> type_err "op_neg"

and op_set ctxs =               (* : *)
  let q = Rt.pop () in
  let v = Rt.pop_get ctxs in
  match ctxs, q, v with
  | ctx :: _, Q x, y ->
     Rt.bind_ctx ctx x y;
     Rt.push y
  | _ -> type_err ":"

and op_apply ctxs =             (* . *)
  match Rt.pop_get ctxs with
  | F { is_oper = _; instrs = _ } as f -> Rt.call_fn f (Rt.create_ctx () :: ctxs)
  | _ -> type_err "."

and op_list_end _ = Rt.push N   (* ] *)
and op_list_begin _ =           (* [ *)
  let xs = Array.empty () in
  let rec go () =
    match Rt.pop () with
    | N -> Rt.push @ L xs
    | x -> Array.add_one xs x; go () in
  go ()

and op_fold ctxs =              (* / *)
  let f = Rt.pop_get ctxs in
  let x = Rt.pop_get ctxs in
  match f, x with
  | F { is_oper = _; instrs = _} as f, L xs ->
     let fn b a =
       (match b, a with
        | N, y -> y
        | x, y -> Rt.push y; Rt.push x; Rt.call_fn f ctxs; Rt.pop ()) in
     Rt.push @ Array.fold_left fn N xs
  | _ -> type_err "/"

and op_scan ctxs =              (* \ *)
  let f = Rt.pop_get ctxs in
  let x = Rt.pop_eval ctxs in
  match f, x with
  | F { is_oper = _; instrs = _} as f, L xs ->
     let fn b a =
       (match b, a with
        | N, y -> Rt.push y; y
        | x, y ->
           Rt.push y; Rt.push x;
           Rt.call_fn f ctxs; Rt.peek ()) in
     with_list ctxs (fun () -> let _ = Array.fold_left fn N xs in ());
     op_rev ctxs;
  | _ -> type_err "/"

and op_dup _ = Rt.push @ Rt.peek ()

and op_rev ctxs =
  match Rt.pop_get ctxs with
  | L xs ->
     let ys = Array.empty () in
     let rec go idx =
       if idx = -1 then L ys
       else (
         Array.add_one ys xs.(idx);
         go (idx - 1)
       ) in
     Rt.push @ go (Array.length xs - 1)
  | _ -> type_err "rev"
  
and op_map ctxs =               (* ' *)
  let f = Rt.pop_get ctxs in
  let x = Rt.pop_get ctxs in
  match f, x with
  | F { is_oper = _; instrs = _} as f, L xs ->
     with_list ctxs @ 
       fun () ->
       let rec go idx =
         if idx = -1 then ()
         else (
           Rt.push @ Array.get xs idx;
           Rt.call_fn f ctxs;
           go (idx - 1)
         ) in
       go @ Array.length xs - 1
  | _ -> type_err "'"

and op_map2 ctxs =              (* '' *)
  let f = Rt.pop_get ctxs in
  let x = Rt.pop_get ctxs in
  let y = Rt.pop_get ctxs in
  match f, x, y with
  | F { is_oper = _; instrs = _} as f, L xs, L ys ->
     map2 ctxs xs ys (Rt.call_fn f)
  | _ -> type_err "''"

let builtin =
  [("+",        true,   op_add);
   ("-",        true,   op_sub);
   ("*",        true,   op_mul);
   ("%",        true,   op_div);
   ("neg",      false,  op_neg);
   (":",        true,   op_set);
   (".",        true,   op_apply);
   ("]",        false,  op_list_end);
   ("[",        false,  op_list_begin);
   ("/",        true,   op_fold);
   ("\\",       true,   op_scan);
   ("'",        true,   op_map);
   ("''",       true,   op_map2);
   ("rev",      false,  op_rev);
   ("dup",      false,  op_dup)]
