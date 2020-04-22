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

and map ctxs xs f =
  with_list ctxs @
    fun () ->
    let rec go idx =
      if idx = -1 then ()
      else (
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
  let y = Rt.pop () in
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
  let y = Rt.pop () in
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
  let y = Rt.pop () in
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
  let y = Rt.pop () in
  match x, y with
  | Z x, Z y -> Rt.push @ Z (x / y)
  | Z x, R y -> Rt.push @ R (float_of_int x /. y)
  | R x, Z y -> Rt.push @ R (x /. float_of_int y)
  | R x, R y -> Rt.push @ R (x /. y)
  | L xs, L ys -> map2 ctxs xs ys op_div
  | x, L ys -> broadcast ~rev:false ctxs x ys op_div
  | L ys, x -> broadcast ~rev:true ctxs x ys op_div
  | _ -> type_err "%"

and op_neg _ =               (* neg *)
  Rt.push @
    match Rt.pop () with
    | Z x -> Z (-1 * x)
    | R x -> R (-1.0 *. x)
    | _ -> type_err "op_neg"

and op_set ctxs =               (* : *)
  match Rt.pop () with
  | Q _ as q -> Rt.bind ctxs q (Rt.peek ())
  | F _ as f ->
     Rt.call_fn f ctxs;
     (match Rt.pop () with
      | L qs -> Array.iteri (fun idx q -> Rt.bind ctxs q (Rt.get idx)) qs
      | _ -> type_err ":")
  | _ ->  type_err ":"

and op_set2 ctxs =              (* :: *)
  match Rt.pop () with
  | Q _ as q -> Rt.bind ctxs q (Rt.pop ())
  | F _ as f ->
     Rt.call_fn f ctxs;
     (match Rt.pop () with
      | L qs -> Array.iter (fun q -> Rt.bind ctxs q (Rt.pop ())) qs
      | _ -> type_err "::")
  | _ ->  type_err "::"

and op_get ctxs =               (* @ *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  match x, y  with
  | Z idx, L xs -> Rt.push @ xs.(idx mod Array.length xs)
  | L idxs, L xs ->
       Rt.push @
         L (Array.map (function | Z(idx) -> xs.(idx) | _ -> type_err ".") idxs)
  | _ -> type_err "@"

and op_apply ctxs =             (* . *)
  match Rt.pop_get ctxs with
  | F _ as f -> Rt.call_fn f (Rt.create_ctx () :: ctxs)
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
  let x = Rt.pop () in
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
  let x = Rt.pop () in
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

and op_dup _ = Rt.push @ Rt.peek () (* dup *)

and op_rev _ =
  match Rt.pop () with
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
  let x = Rt.pop () in
  match f, x with
  | F _ as f, L xs ->
     with_list ctxs @ 
       fun () ->
       let rec go idx =
         if idx = -1 then ()
         else (
           Rt.push xs.(idx);
           Rt.call_fn f ctxs;
           go (idx - 1)
         ) in
       go @ Array.length xs - 1
  | _ -> type_err "'"

and op_map2 ctxs =              (* '' *)
  let f = Rt.pop_get ctxs in
  let x = Rt.pop () in
  let y = Rt.pop () in
  match f, x, y with
  | F _ as f, L xs, L ys -> map2 ctxs xs ys (Rt.call_fn f)
  | _ -> type_err "''"

and op_drop _ = let _ = Rt.pop () in () (* drop *)
and op_swap _ = Array.swap Rt.stk (Rt.convert 0) (Rt.convert 1) (* swap *)

and op_til _ =                  (* til *)
  match Rt.pop () with
  | Z x ->
     let xs = Array.empty () in
     let rec go idx =
       if idx = x then L xs
       else (
         Array.add_one xs (Z idx);
         go (idx + 1)
       ) in
     Rt.push @ go 0
  | _ -> type_err "til"

and op_eq ctxs =                (* = *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  Rt.push @ B (xs_eq x y)

and op_trues ctxs =             (* ~ *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  match x, y with
  | L xs, L ys -> map2 ctxs xs ys op_eq
  | x, L ys -> broadcast ~rev:false ctxs x ys op_eq
  | L xs, y -> broadcast ~rev:true ctxs y xs op_eq
  | x, y -> Rt.push @ B (xs_eq x y)

and op_lt ctxs =                (* < *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  match x, y with
  | L xs, L ys -> map2 ctxs xs ys op_lt
  | x, L ys -> broadcast ~rev:false ctxs x ys op_lt
  | L xs, y -> broadcast ~rev:true ctxs y xs op_lt
  | Z x, Z y -> Rt.push @ B (x < y)
  | Z x, R y -> Rt.push @ B (Float.(<) (float_of_int x) y)
  | R x, R y -> Rt.push @ B (Float.(<) x y)
  | R x, Z y -> Rt.push @ B (Float.(<) x (float_of_int y))
  | _ -> type_err "<"

and op_gt ctxs =                (* > *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  match x, y with
  | L xs, L ys -> map2 ctxs xs ys op_gt
  | x, L ys -> broadcast ~rev:false ctxs x ys op_gt
  | L xs, y -> broadcast ~rev:true ctxs y xs op_gt
  | Z x, Z y -> Rt.push @ B (x > y)
  | Z x, R y -> Rt.push @ B (Float.(>) (float_of_int x) y)
  | R x, R y -> Rt.push @ B (Float.(>) x y)
  | R x, Z y -> Rt.push @ B (Float.(>) x (float_of_int y))
  | _ -> type_err "<"

and op_geq ctxs =               (* gq *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  match x, y with
  | L xs, L ys -> map2 ctxs xs ys op_geq
  | x, L ys -> broadcast ~rev:false ctxs x ys op_geq
  | L xs, y -> broadcast ~rev:true ctxs y xs op_geq
  | Z x, Z y -> Rt.push @ B (x >= y)
  | Z x, R y -> Rt.push @ B (Float.(>=) (float_of_int x) y)
  | R x, R y -> Rt.push @ B (Float.(>=) x y)
  | R x, Z y -> Rt.push @ B (Float.(>=) x (float_of_int y))
  | _ -> type_err "<"

and op_leq ctxs =               (* lq *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  match x, y with
  | L xs, L ys -> map2 ctxs xs ys op_leq
  | x, L ys -> broadcast ~rev:false ctxs x ys op_leq
  | L xs, y -> broadcast ~rev:true ctxs y xs op_leq
  | Z x, Z y -> Rt.push @ B (x <= y)
  | Z x, R y -> Rt.push @ B (Float.(<=) (float_of_int x) y)
  | R x, R y -> Rt.push @ B (Float.(<=) x y)
  | R x, Z y -> Rt.push @ B (Float.(<=) x (float_of_int y))
  | _ -> type_err "<"

and op_if ctxs =                (* if *)
  let cond = Rt.pop () in
  let x = Rt.pop () in
  let y = Rt.pop () in
  match cond, x, y with
  | B b, (F _ as fx), (F _ as fy) ->
     Rt.call_fn (if Bool.equal b true then fx else fy) ctxs
  | _ -> type_err "if"

and op_cond ctxs =              (* cond *)
  match Rt.pop () with
  | L xs ->
     let len = Array.length xs in
     if len mod 2 = 0 then
       raise @ Failure "cond list's length must be odd"
     else
       let rec go idx =
         if idx = len - 1 then
           Rt.call_fn xs.(len - 1) ctxs
         else (
           Rt.call_fn xs.(idx) ctxs;
           match Rt.pop () with
           | B true -> Rt.call_fn xs.(idx + 1) ctxs 
           | B false -> go (idx + 2)
           | _ -> type_err "cond"
         ) in go 0
  | _ -> type_err "cond"

and op_concat ctxs =            (* , *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  match x, y with
  | L xs, L ys -> Rt.push @ L (Array.append xs ys)
  | _ -> type_err ","

and op_cons ctxs =            (* ,, *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  match x, y with
  | x, L ys -> Rt.push @ L (Array.append (Array.create 1 x) ys)
  | L xs, y -> Rt.push @ L (Array.append xs (Array.create 1 y))
  | _ -> type_err ","

and op_count _ =             (* count *)
  let x = Rt.pop () in
  match x with
  | L xs -> Rt.push @ Z (Array.length xs)
  | _ -> type_err "count"


let builtin =
  [("+",        true,   op_add);
   ("-",        true,   op_sub);
   ("*",        true,   op_mul);
   ("%",        true,   op_div);
   (".",        true,   op_apply);
   ("/",        true,   op_fold);
   ("\\",       true,   op_scan);
   ("'",        true,   op_map);
   ("@",        true,   op_get);
   ("''",       true,   op_map2);
   (":",        true,   op_set);
   ("::",       true,   op_set2);
   ("=",        true,   op_eq);
   ("~",        true,   op_trues);
   ("<",        true,   op_lt);
   (">",        true,   op_gt);
   ("gq",       true,   op_geq);
   ("lq",       true,   op_leq);
   (",",        true,   op_concat);
   (",,",       true,   op_cons);
   ("if",       false,  op_if);
   ("cond",     false,  op_cond);
   ("]",        false,  op_list_end);
   ("[",        false,  op_list_begin);
   ("neg",      false,  op_neg);   
   ("rev",      false,  op_rev);
   ("dup",      false,  op_dup);
   ("drop",     false,  op_drop);
   ("swap",     false,  op_swap);
   ("til",      false,  op_til);
   ("count",    false,  op_count)
  ]
