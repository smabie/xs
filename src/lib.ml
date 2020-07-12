(* lib.ml *)
(*
 * This file is public domain as declared by Sturm Mabie
 *)

open Core
open Defs

module RArray = Res.Array

let rec with_list ctxs f = op_list_end ctxs; f (); op_list_begin ctxs
and with_list_rev ctxs f = op_list_end ctxs; f (); op_list_begin_rev ctxs

(* it doesn't matter whether is_oper is true or false at this stage *)
and make_builtin f = F { is_oper = false; instrs = Builtin f }

and map2 ctxs xs ys f =
  if Array.length xs <> Array.length ys then
    failwith "list length unequal"
  else
    with_list_rev ctxs @
      fun () ->
      let rec go ix =
        if ix = Array.length xs then ()
        else (
          Rt.push ys.(ix);
          Rt.push xs.(ix);
          f ctxs;
          go (ix + 1)
        ) in
      go 0

and map ctxs xs f =
  with_list_rev ctxs @
    fun () ->
    let rec go ix =
      if ix = Array.length xs then ()
      else (
        Rt.push xs.(ix);
        f ctxs;
        go (ix + 1)
      ) in
    go 0

and broadcast  ~rev ctxs x ys f =
  with_list_rev ctxs @
    fun () ->
    let rec go ix =
      if ix = Array.length ys then ()
      else (
        if rev then (
          Rt.push x;
          Rt.push ys.(ix)
        ) else (
          Rt.push ys.(ix);
          Rt.push x;
        );
        f ctxs;
        go (ix + 1)
      ) in
    go 0

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
  | _ -> type_err "+"

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

and op_mod ctxs =               (* mod *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  match x, y with
  | Z x, Z y -> Rt.push @ Z (x mod y)
  | L xs, L ys -> map2 ctxs xs ys op_div
  | x, L ys -> broadcast ~rev:false ctxs x ys op_mod
  | L ys, x -> broadcast ~rev:true ctxs x ys op_mod
  | _ -> type_err "mod"

and op_neg _ =                  (* neg *)
  Rt.push @
    match Rt.pop () with
    | Z x -> Z (-1 * x)
    | R x -> R (-1.0 *. x)
    | _ -> type_err "op_neg"

and op_set ctxs =               (* ~ *)
  match Rt.pop () with
  | Q _ as q -> Rt.bind ctxs q (Rt.peek ())
  | F _ as f ->
     Rt.call_fn f ctxs;
     (match Rt.pop () with
      | L qs -> Array.iteri qs (fun ix q -> Rt.bind ctxs q (Rt.get ix))
      | _ -> type_err ":")
  | N -> print_endline @ Xs.to_string (Rt.peek ())
  | _ ->  type_err "~"

and op_set2 ctxs =              (* : *)
  match Rt.pop () with
  | Q _ as q -> Rt.bind ctxs q (Rt.pop ())
  | F _ as f ->
     Rt.call_fn f ctxs;
     (match Rt.pop () with
      | L qs -> Array.iter qs (fun q -> Rt.bind ctxs q (Rt.pop ()))
      | _ -> type_err "::")
  | N -> print_endline @ Xs.to_string (Rt.pop ())
  | _ ->  type_err ":"

and op_reassign ctxs =          (* :: *)
  let set s x =
    match Rt.find_ctx ctxs s with
    | Some ctx -> Hashtbl.set ctx s x
    | None -> failwith "::, no variable found to reassign" in
  match Rt.pop () with
  | Q x -> set x @ Rt.pop ()
  | F _ as f ->
     Rt.call_fn f ctxs;
     (match Rt.pop () with
      | L qs -> Array.iter qs (function | Q x -> set x @ Rt.pop () | _ -> type_err "::")
      | _ -> type_err "::")
  | _ -> type_err "::"

and op_apply ctxs =             (* . *)
  match Rt.pop_get ctxs with
  | F _ as f -> Rt.call_fn f (Rt.create_ctx () :: ctxs)
  | x -> Rt.push x

and op_list_end _ = Stack.push Rt.xstk @ Rt.len ()

and op_list_begin _ =           (* [ *)
  match Stack.pop Rt.xstk with
  | Some x ->
     let n = Rt.len () - x in
     Rt.push @ L (Array.init n (fun _ -> Rt.pop ()))
  | None ->  failwith "Cannot find stack begin marker"

and op_list_begin_rev _ =
  match Stack.pop Rt.xstk with
  | Some x ->
     let n = Rt.len () - x in
     let xs = Array.init n (fun ix -> Rt.get (n - ix - 1)) in
     Res.Array.remove_n Rt.stk n;
     Rt.push @ L xs;
  | None -> failwith "Cannot find stack begin marker"

and op_fold ctxs =              (* / *)
  let f = Rt.pop_get ctxs in
  let x = Rt.pop () in
  match f, x with
 | F _ as f, L xs ->
     let len = Array.length xs in
     let rec go ix =
       if ix = len then ()
       else (
         Rt.push xs.(ix);
         Rt.swap 0 1;
         Rt.call_fn f ctxs;
         go (ix + 1)
       ) in
     Rt.push xs.(0);
     go 1
  | F _ as f, x ->
     let rec go prev =
       Rt.call_fn f ctxs;
       let y = Rt.peek () in
       if Xs.equal x y || Xs.equal prev y then ()
       else go y in
     Rt.push x;
     Rt.call_fn f ctxs;
     let y = Rt.peek () in
     if Xs.equal y x then ()
     else go y
  | _ -> type_err "/"

and op_fix ctxs =               (* fix *)
  let f = Rt.pop_get ctxs in
  let x = Rt.pop () in
  match f, x with
  | F _ as f, x ->
     let rec go prev =
       Rt.call_fn f ctxs;
       let y = Rt.peek () in
       if Xs.equal x y || Xs.equal prev y then ()
       else go y in
     Rt.push x;
     Rt.call_fn f ctxs;
     let y = Rt.peek () in
     if Xs.equal y x then ()
     else go y
  | _ -> type_err "fix"

and op_fixes ctxs =               (* fixes *)
  let f = Rt.pop_get ctxs in
  let x = Rt.pop () in
  match f, x with
  | F _ as f, x ->
     with_list_rev ctxs @
       fun () ->
       let rec go prev =
         Rt.dup ();
         Rt.call_fn f ctxs;
         let y = Rt.peek () in
         if Xs.equal x y || Xs.equal prev y then
           let _ = Rt.pop () in  ()
         else
           go y in
       Rt.push x;
       Rt.dup ();
       Rt.call_fn f ctxs;
       let y = Rt.peek () in
       if Xs.equal y x then
         let _ = Rt.pop () in ()
       else go y
  | _ -> type_err "fix"

and op_scan ctxs =              (* \ *)
  let f = Rt.pop_get ctxs in
  let x = Rt.pop () in
  match f, x with
  | F _ as f, L xs ->
     let len = Array.length xs in
     let ys =
       Array.init len
         (fun ix ->
           if ix = 0 then (
             Rt.push xs.(ix);
             xs.(ix)
           ) else (
             Rt.push xs.(ix);
             Rt.swap 0 1;
             Rt.call_fn f ctxs;
             if ix = len - 1 then Rt.pop () else Rt.peek ()
           )
         ) in
     Rt.push @ L ys
  | _ -> type_err "/"

and op_dup _ = Rt.dup () (* dup *)

and op_rev _ =
  Rt.push @
    match Rt.pop () with
    | L xs ->
       let len = Array.length xs in
       L (Array.init len (fun ix -> xs.(len - ix - 1)))
    | S x -> S (String.rev x)
    | _ -> type_err "rev"

and op_map ctxs =               (* ' *)
  let f = Rt.pop_get ctxs in
  let x = Rt.pop () in
  match f, x with
  | F _ as f, L xs ->
     with_list ctxs @
       fun () ->
       let rec go ix =
         if ix = -1 then ()
         else (
           Rt.push xs.(ix);
           Rt.call_fn f ctxs;
           go (ix - 1)
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

and op_drop_stk _ = let _ = Rt.pop () in () (* drop *)
and op_swap _ = Rt.swap 0 1                 (* swap *)

and op_til _ =                  (* til *)
  match Rt.pop () with
  | Z x -> Rt.push @ L (Array.init x (fun ix -> Z ix))
  | _ -> type_err "til"

and op_eq ctxs =                (* == *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  Rt.push @ B (Xs.equal x y)

and op_trues ctxs =             (* = *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  match x, y with
  | L xs, L ys -> map2 ctxs xs ys op_eq
  | x, L ys -> broadcast ~rev:false ctxs x ys op_eq
  | L xs, y -> broadcast ~rev:true ctxs y xs op_eq
  | x, y -> Rt.push @ B (Xs.equal x y)

and op_lt ctxs =                (* < *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  match x, y with
  | L xs, L ys -> map2 ctxs xs ys op_lt
  | x, L ys -> broadcast ~rev:false ctxs x ys op_lt
  | L xs, y -> broadcast ~rev:true ctxs y xs op_lt
  | x, y -> Rt.push @ B (Xs.compare x y = -1)

and op_gt ctxs =                (* > *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  match x, y with
  | L xs, L ys -> map2 ctxs xs ys op_gt
  | x, L ys -> broadcast ~rev:false ctxs x ys op_gt
  | L xs, y -> broadcast ~rev:true ctxs y xs op_gt
  | x, y -> Rt.push @ B (Xs.compare x y = 1)

and op_geq ctxs =               (* gq *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  match x, y with
  | L xs, L ys -> map2 ctxs xs ys op_geq
  | x, L ys -> broadcast ~rev:false ctxs x ys op_geq
  | L xs, y -> broadcast ~rev:true ctxs y xs op_geq
  | x, y ->
     let c = Xs.compare x y in
     Rt.push @ B (c = 1 || c = 0 )

and op_leq ctxs =               (* lq *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  match x, y with
  | L xs, L ys -> map2 ctxs xs ys op_leq
  | x, L ys -> broadcast ~rev:false ctxs x ys op_leq
  | L xs, y -> broadcast ~rev:true ctxs y xs op_leq
  | x, y ->
     let c = Xs.compare x y in
     Rt.push @ B (c = -1 || c = 0 )

and op_if ctxs =                (* if *)
  let cond = Rt.pop () in
  let x = Rt.pop () in
  let y = Rt.pop () in
  match cond, x, y with
  | B b, (F _ as fx), (F _ as fy) ->
     Rt.call_fn (if Bool.equal b true then fx else fy) ctxs
  | _ -> type_err "if"

and op_cond ctxs =              (* cond *)
  let name = "cond" in
  match Rt.pop () with
  | L xs ->
     let len = Array.length xs in
     if len mod 2 = 0 then
       failwith "cond list's length must be odd"
     else
       let rec go ix =
         if ix = len - 1 then
           Rt.call_fn xs.(len - 1) ctxs
         else (
           Rt.call_fn xs.(ix) ctxs;
           match Rt.pop () with
           | B true -> Rt.call_fn xs.(ix + 1) ctxs
           | B false -> go (ix + 2)
           | _ -> type_err name
         ) in
       go 0
  | _ -> type_err name

and op_concat ctxs =            (* , *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  Rt.push @
    match x, y with
    | L xs, L ys -> L (Array.append xs ys)
    | x, L ys -> L (Array.append [|x|] ys )
    | L xs, y -> L (Array.append xs [|y|])
    | S x, S y -> S (x ^ y)
    | x, y -> L [|x; y|]

and op_cons ctxs =            (* ,, *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  match x, y with
  | x, L ys -> Rt.push @ L (Array.append (Array.create 1 x) ys)
  | L xs, y -> Rt.push @ L (Array.append xs (Array.create 1 y))
  | _ -> type_err ","

and op_len _ =             (* len *)
  let x = Rt.pop () in
  Rt.push @
    match x with
    | L xs -> Z (Array.length xs)
    | S x -> Z (String.length x)
    | _ -> type_err "len"

and op_take ctxs =              (* # *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  match x, y with
  | Z x, L xs ->
     let len, nlen = Array.length xs, abs x in
     let ys = Array.create nlen N in
     let rec go ix c =
       if c = nlen then Rt.push @ L ys
       else (
         ys.(c) <- xs.((if ix < 0 then (len + ix) else ix) mod len);
         go (ix + 1) (c + 1)
       ) in
     if x > 0 then go 0 0 else go (len + x) 0
  | Z x, S y ->
     let len, nlen = String.length y, abs x in
     let ys = Array.create nlen '0' in
     let rec go ix c =
       if c = nlen then Rt.push @ S (String.of_char_list (Array.to_list ys))
       else (
         ys.(c) <- String.get y @ (if ix < 0 then
                                     len + ix else ix) mod len;
         go (ix + 1) (c + 1)
       ) in
     if x > 0 then go 0 0 else go (len + x) 0
  | Z x, y -> Rt.push @ L (Array.create (abs x) y)
  | _ -> type_err "#"

and op_get ctxs =               (* @ *)
  let name = "@" in
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  Rt.push @
    match x, y with
    | Z ix, L ys -> ys.(ix mod Array.length ys)
    | L ixs, L ys ->
       L (Array.map ixs
            (function
             | Z ix -> ys.(ix mod Array.length ys)
             | _ -> type_err name))
    | Z ix, S x -> S (Char.to_string @ String.get x ix)
    | L ixs, S x ->
       L (Array.map ixs
            (function
             | Z ix -> S (Char.to_string @ String.get x (ix mod String.length x))
             | _ -> type_err name))
    | _ -> type_err name

and op_find ctxs =              (* ? *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  let find_ix x xs =
    Z (match Array.findi xs (fun _ y -> Xs.equal x y) with
       | Some (ix, _) -> ix
       | None -> Array.length xs) in
  Rt.push @
    match x, y with
    | L xs, L ys ->
       L (Array.init (Array.length xs)
            (fun ix -> find_ix xs.(ix) ys))
    | x, L xs -> find_ix x xs
    | S x, S y ->
       L (Array.init (String.length x)
            (fun ix ->
              match String.lfindi ~pos:0 y
                      ~f:(fun _ yc -> Char.equal (String.get x ix) yc) with
              | Some x -> Z x
              | None -> Z (String.length y)))
  | _ -> type_err "?"

and op_sum ctxs =               (* sum *)
  Rt.push @ make_builtin op_add;
  op_fold ctxs;

and op_prod ctxs =              (* prod *)
  Rt.push @ make_builtin op_mul;
  op_fold ctxs;

and op_sums ctxs =              (* sums *)
  Rt.push @ make_builtin op_add;
  op_scan ctxs;

and op_prods ctxs =             (* prods *)
  Rt.push @ make_builtin op_mul;
  op_scan ctxs;

and op_where _ =                (* where *)
  let name = "where" in
  match Rt.pop () with
  | L xs ->
     let n =
       Array.fold xs ~init:0
         ~f:(fun x y ->
           match x, y with
           | x, Z y -> x + y
           | x, B y -> x + Bool.to_int y
           | _ -> type_err name) in
     let ys = Array.create n N in
     let rec go iy ix =
       if ix = Array.length xs then L ys
       else (
         let x =
           match xs.(ix) with
           | Z x -> x
           | B x -> Bool.to_int x
           | _ -> type_err name in
         for i = iy to iy + x - 1 do
           ys.(i) <- Z ix
         done;
         go (iy + x) (ix + 1)
       ) in
     Rt.push @ go 0 0
  | _ -> type_err name

and op_drop ctxs =              (* _ *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  let bounds x len =
    let s = if x > 0 then x else 0 in
    let e = if x > 0 then 0 else len + x in
    s, e in
  Rt.push @
    match x, y with
    | Z x, L ys when abs x < Array.length ys ->
       let s, e = bounds x @ Array.length ys in L (Array.slice ys s e)
    | Z x, S y when abs x < String.length y ->
       let s, e = bounds x @ String.length y in S (String.slice y s e)
    | Z _, L _ -> L [||]
    | Z _, S _ -> S ""
    | _ -> type_err "_"

and op_pow ctxs =               (* ** *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  match x, y with
  | Z x, Z y -> Rt.push @ Z (int_of_float @ (float_of_int x) ** (float_of_int y))
  | Z x, R y -> Rt.push @ R ((float_of_int x) ** y)
  | R x, Z y -> Rt.push @ R (x ** (float_of_int y))
  | R x, R y -> Rt.push @ R (x ** y)
  | x, L ys -> broadcast ~rev:false ctxs x ys op_pow
  | _ -> type_err "**"

and op_ln ctxs =                (* ln *)
  match Rt.pop () with
  | Z x -> Rt.push @ R (Float.log @ float_of_int x)
  | R x -> Rt.push @ R (Float.log x)
  | L xs -> map ctxs xs op_ln
  | _ -> type_err "ln"

and op_sin ctxs =               (* sin *)
  match Rt.pop () with
  | Z x -> Rt.push @ R (Float.sin @ float_of_int x)
  | R x -> Rt.push @ R (Float.sin x)
  | L xs -> map ctxs xs op_sin
  | _ -> type_err "sin"

and op_cos ctxs =               (* cos *)
  match Rt.pop () with
  | Z x -> Rt.push @ R (Float.cos @ float_of_int x)
  | R x -> Rt.push @ R (Float.cos x)
  | L xs -> map ctxs xs op_cos
  | _ -> type_err "cos"

and op_tan ctxs =               (* tan *)
  match Rt.pop () with
  | Z x -> Rt.push @ R (Float.tan @ float_of_int x)
  | R x -> Rt.push @ R (Float.tan x)
  | L xs -> map ctxs xs op_cos
  | _ -> type_err "tan"

and op_ceil ctxs =              (* ceil *)
  match Rt.pop () with
  | Z x -> Rt.push @ Z x
  | R x -> Rt.push @ Z (int_of_float @ Float.round_up x)
  | L xs -> map ctxs xs op_ceil
  | _ -> type_err "ceil"

and op_floor ctxs =             (* floor *)
  match Rt.pop () with
  | Z x -> Rt.push @ Z x
  | R x -> Rt.push @ Z (int_of_float @ Float.round_down x)
  | L xs -> map ctxs xs op_floor
  | _ -> type_err "floor"

and op_abs ctxs =               (* abs *)
  match Rt.pop () with
  | Z x -> Rt.push @ Z (Int.abs x)
  | R x -> Rt.push @ R (Float.abs x)
  | L xs -> map ctxs xs op_abs
  | _ -> type_err "abs"

and op_enlist ctxs =            (* enlist *)
  match Rt.pop_eval ctxs with
  | Z n -> Rt.push @ L (Array.init n (fun _ -> Rt.pop ()))
  | _ -> type_err "enlist"

and op_readl _ =                (* readl *)
  Rt.push @
    match Rt.pop () with
    | Z 0 ->
       L (In_channel.(
            fold_lines ~fix_win_eol:true stdin
              ~init:[] ~f:(fun xs s -> S s :: xs)) |>
            List.rev |>
            List.to_array)
    | S x ->
       In_channel.read_lines ~fix_win_eol:true x |>
         Array.of_list_map ~f:(fun x -> S x) |>
         fun x -> L x
    | _ -> type_err "readl"

and op_writel ctxs =            (* writel *)
  let name = "writel" in
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  let print xs ch =
    let ys = Array.to_list @
               Array.map xs
                 (function
                  | S x -> x
                  | _ -> type_err name) in
    Out_channel.output_lines ch ys in
  match x, y with
  | L xs, Z 1 -> print xs stdout
  | L xs, Z 2 -> print xs stderr
  | S x, Z 1 -> Out_channel.(output_string stdout x)
  | S x, Z 2 -> Out_channel.(output_string stderr x)
  | L ys, S x ->
     Array.to_list ys |>
       List.map ~f:(function | S x -> x | _ -> type_err name) |>
       Out_channel.write_lines x
  | _ -> type_err name

and op_measure ctxs =           (* measure *)
  match Rt.pop () with
  | F _ as f ->
     let t = Unix.gettimeofday () in
     Rt.call_fn f ctxs;
     Rt.push @ R (Unix.gettimeofday () -. t)
  | _ -> type_err "measure"

and op_sv ctxs =                (* sv *)
  let name = "sv" in
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  match x, y with
  | S x, L ys ->
     Rt.push @
       S (Array.fold ys ~init:""
            ~f:(fun b a ->
              match b, a with
              | "", S a -> a
              | b, S a -> b ^ x ^ a
              | _ -> type_err name))
  | _ -> type_err name

and op_vs ctxs =                (* vs *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  match x, y with
  | S x, S y ->
     Str.split (Str.regexp x) y |>
       List.map ~f:(fun x -> S x) |>
       Array.of_list |>
       fun x -> Rt.push (L x)
  | _ -> type_err "vs"

and op_cast _ =               (* of *)
  let x = Rt.pop () in
  let y = Rt.pop () in
  Rt.push @
    match x, y with
    | Q ("Z" | "z"), B x -> Z (Bool.to_int x)
    | Q ("Z" | "z"), S x -> Z (int_of_string x)
    | Q ("Z" | "z"), R x -> Z (int_of_float x)
    | Q ("R" | "r"), B x -> R (Int.to_float @ Bool.to_int x)
    | Q ("R" | "r"), S x -> R (float_of_string x)
    | Q ("R" | "r"), Z x -> R (float_of_int x)
    | Q ("Q" | "q"), S x -> Q x
    | Q ("S" | "s"), Q x -> S x
    | Q ("S" | "s"), Z x -> S (Int.to_string x)
    | Q ("S" | "s"), R x -> S (Float.to_string x)
    | Q ("L" | "l"), S x ->
       L (Array.map (String.to_array x)
            ~f:(fun x -> S (Char.to_string x)))
    | _ -> type_err "of"

and op_in ctxs =                   (* in *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  let exists x xs = Array.exists xs ~f:(Xs.equal x) in
  Rt.push @
    match x, y with
    | L xs, L ys ->
       L (Array.init (Array.length xs)
            (fun ix -> B (exists xs.(ix) ys)))
    | S x, S y ->
       L (Array.init (String.length x)
            (fun ix -> B (String.contains y @ String.get x ix)))
    | x, L ys -> B (exists x ys)
    | _ -> type_err "in"

and op_inter ctxs =             (* inter *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  Rt.push @
    match x, y with
    | L xs, L ys ->
       let x, y = Set.of_array (module Xs) xs,
                  Set.of_array (module Xs) ys in
       L (Set.to_array @ Set.inter x y)
    | S x, S y ->
       S (String.filter x (fun c -> String.contains y c))
    | x, y when Xs.equal x y ->  L [|x|]
    | _, _ -> L [||]

and op_uniq _ =                 (* uniq *)
  Rt.push @
    match Rt.pop () with
    | L xs -> L (Set.to_array @ Set.of_array (module Xs) xs)
    | S x ->
       S (String.of_char_list @ Set.to_list @
            Set.of_array (module Char) @ String.to_array x)
    | x -> L [|x|]

and op_union ctxs =             (* union *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  Rt.push @
    match x, y with
    | L xs, L ys ->
       let x, y = Set.of_array (module Xs) xs, Set.of_array (module Xs) ys in
       L (Set.to_array @ Set.union x y)
    | S x, S y ->
       let x, y = Set.of_array (module Char) @ String.to_array x,
                  Set.of_array (module Char) @ String.to_array y in
       S (String.of_char_list @ Set.to_list @ Set.union x y )
    | L xs, y | y, L xs ->
       L (Set.to_array @ Set.of_list (module Xs) @ y :: Array.to_list xs)
    | x, y when Xs.equal x y -> L [|x|]
    | x, y -> L [|x; y|]

and op_swap_apply ctxs =        (* $ *)
  match Rt.pop_get ctxs with
  | F _ as f ->
     Rt.swap 0 1;
     Rt.call_fn f ctxs;
  | _ -> type_err "$"

and op_type _ =              (* type *)
  Rt.push @
    match Rt.pop () with
    | Z _ -> Q "Z"
    | R _ -> Q "R"
    | B _ -> Q "B"
    | S _ -> Q "S"
    | L _ -> Q "L"
    | Q _ -> Q "Q"
    | F _ -> Q "F"
    | H _ -> Q "H"
    | N -> Q "N"

and op_cut ctxs =               (* cut *)
  let name = "cut" in
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  let len x len = Int.of_float @ Float.(round_up @ (of_int @ len) / of_int x) in
  Rt.push @
    match x, y with
    | Z x, L ys when x > 0 ->
       let nlen = len x (Array.length ys) in
       L (Array.init nlen
            (fun ix ->
              let e = if ix = nlen - 1 then 0 else ix * x + x in
              L (Array.slice ys (ix * x) e)))
    | Z x, S y when x > 0 ->
       let nlen = len x (String.length y) in
       L (Array.init nlen
            (fun ix ->
              let e =  if ix = nlen - 1 then 0 else ix * x + x in
              S (String.slice y (ix * x) e)))
    | L ixs, L xs ->
       let len = Array.length ixs in
       L (Array.init len
            (fun ix ->
              let s, e =
                if ix = len -1 then
                  match ixs.(ix) with
                  | Z x -> x, 0
                  | _ -> type_err name
                else
                  match ixs.(ix), ixs.(ix + 1) with
                  | Z x, Z y -> x, y
                  | _ -> type_err name in
              L (Array.slice xs s e)))
    | L ixs, S x ->
       let len = Array.length ixs in
       L (Array.init len
            (fun ix ->
              let s, e =
                if ix = len -1 then
                  match ixs.(ix) with
                  | Z x -> x, 0
                  | _ -> type_err name
                else
                  match ixs.(ix), ixs.(ix + 1) with
                  | Z x, Z y -> x, y
                  | _ -> type_err name in
              S (String.slice x s e)))
    | _ -> type_err name

and op_flip _ =                 (* flip *)
  let name = "flip" in
  let x = Rt.pop () in
  Rt.push @
    match x with
    | L xs when Array.length xs > 0 ->
       (match xs.(0) with
        | L ys ->
           let xlen = Array.length ys in
           let ylen = Array.length xs in
           L (Array.init xlen
                (fun ix ->
                  L (Array.init ylen
                    (fun iy ->
                      match xs.(iy) with
                      | L zs -> zs.(ix)
                      | _ -> type_err name))))
        | S y ->
           let xlen = String.length y in
           let ylen = Array.length xs in
           L (Array.init xlen
                (fun ix ->
                  S (String.init ylen
                       (fun iy ->
                         match xs.(iy) with
                         | S z -> String.get z ix
                         | _ -> type_err name))))
        | _ -> L (Array.map xs (fun x -> L [|x|])))
    | S x -> L (Array.map (String.to_array x) (fun c -> S (Char.to_string c)))
    | _ -> type_err name

and op_min _ =                  (* min*)
  Rt.push @
    match Rt.pop () with
    | L xs ->
       (match Array.min_elt xs Xs.compare with
        | Some x -> x
        | None -> N)
    | S x ->
       (match String.min_elt x Char.compare with
        | Some x -> S (Char.to_string x)
        | None -> N)
    | x -> x

and op_max _ =                  (* max *)
  Rt.push @
    match Rt.pop () with
    | L xs ->
       (match Array.max_elt xs Xs.compare with
        | Some x -> x
        | None -> N)
    | S x ->
       (match String.max_elt x Char.compare with
        | Some x -> S (Char.to_string x)
        | None -> N)
    | x -> x


and sort_helper f =
  Rt.push @
    match Rt.pop () with
    | L xs -> L (Array.sorted_copy xs f)
    | S x ->
       S (String.of_char_list @
            List.sort (String.to_list x)
              (fun x y -> f (S (String.of_char x)) (S (String.of_char y))))
    | x -> L [|x|]
and op_asc _ = sort_helper Xs.compare (* asc *)
and op_dsc _ = sort_helper (fun x y -> -1 * Xs.compare x y) (* dsc *)

and op_cmp ctxs =               (* cmp *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  Rt.push @ Z (Xs.compare x y)

and op_sort ctxs =              (* sort *)
  let name = "sort" in
  let f = Rt.pop_get ctxs in
  let x = Rt.pop () in
  Rt.push @
    match f, x with
    | F _ as f, L xs ->
       L (Array.sorted_copy xs
            (fun x y ->
              Rt.push y;
              Rt.push x;
              Rt.call_fn f ctxs;
              match Rt.pop () with
              | Z x -> x
              | _ -> type_err name))
    | F _ as f, S x ->
       S (String.of_char_list @
            List.sort (String.to_list x)
              (fun x y ->
                Rt.push @ S (Char.to_string y);
                Rt.push @ S (Char.to_string x);
                Rt.call_fn f ctxs;
                match Rt.pop () with
                | Z x -> x
                | _ -> type_err name))
    | _ -> type_err name

and bool_helper ctxs f g s =
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  match x, y with
  | B x, B y -> Rt.push @ B (f x y)
  | x, L ys -> broadcast ~rev:false ctxs x ys g
  | L xs, y -> broadcast ~rev:true ctxs y xs g
  | _ -> type_err s

and op_or ctxs =  bool_helper ctxs (||) op_or "||" (* || *)
and op_and ctxs = bool_helper ctxs (&&) op_and "&&" (* && *)

and existential_helper _ f s =
  Rt.push @
    match Rt.pop () with
    | L xs -> B (f xs (function | B true -> true | _ -> false))
    | B true as b -> b
    | B false as b -> b
    | _ -> type_err s

and op_any ctxs =               (* any *)
  existential_helper ctxs (fun xs f -> Array.exists xs f) "any"

and op_every ctxs =             (* every *)
  existential_helper ctxs (fun xs f -> Array.for_all xs f) "every"

and op_cat ctxs =               (* cat *)
  Rt.push @ make_builtin(op_concat);
  op_fold ctxs

and op_cats ctxs =              (* cats *)
  Rt.push @ make_builtin(op_concat);
  op_scan ctxs

and op_delist _ =               (* ^ *)
  match Rt.pop () with
  | L xs ->
     let rec go ix =
       if ix = -1 then ()
       else (
         Rt.push @ xs.(ix);
         go (ix - 1)
       ) in
     go @ Array.length xs - 1
  | _ -> type_err "^"

and op_do ctxs =                (* do *)
  let name = "do" in
  let f = Rt.pop_get ctxs in
  let x = Rt.pop () in
  match f, x with
  | Z x, (F _ as f) ->
     let rec go n =
       if n = x then ()
       else (
         Rt.call_fn f ctxs;
         go (n + 1)
       ) in
     go 0
  | (F _ as fx), (F _ as fy) ->
     let rec go () =
       Rt.call_fn fx ctxs;
       (match Rt.pop () with
        | B true ->
           Rt.call_fn fy ctxs;
           go ()
        | B false -> ()
        | _ -> failwith name) in
     go ()
  | _ -> type_err name

and op_open _ =                 (* open *)
  let x = Rt.pop () in
  let f = Rt.pop () in
  Rt.push @
    match x, f with
    | Q ("r" | "R"), S f ->
       let h = H (!Rt.fdnum, In (In_channel.create f)) in
       Rt.fdnum := !Rt.fdnum + 1;
       h
    | Q ("w" | "W"), S f ->
       let h =  H (!Rt.fdnum, Out (Out_channel.create f)) in
       Rt.fdnum := !Rt.fdnum + 1;
       h
    | Q ("a" | "A"), S f ->
       let h =  H (!Rt.fdnum, Out (Out_channel.create f ~append:true)) in
       Rt.fdnum := !Rt.fdnum + 1;
       h
    | _ -> type_err "open"

and op_read ctxs =              (* read *)
  let x = Rt.pop_eval ctxs in
  let h = Rt.pop () in
  let read n ch =
    let buf = Buffer.create n in
    let _ = In_channel.input_buffer ch buf n in
    S (Buffer.contents buf) in
  Rt.push @
    match x, h with
    | Z x, Z 0 -> read x In_channel.stdin
    | Z x, H (_, In ch) -> read x ch
    | _ -> type_err "read"

and op_write ctxs =             (* write *)
  let x = Rt.pop_eval ctxs in
  let h = Rt.pop () in
  let write s ch =
    let buf = Buffer.create @ String.length s in
    Buffer.add_string buf s;
    Out_channel.output_buffer ch buf in
  match x, h with
  | S x, Z 1 -> write x Out_channel.stdout
  | S x, Z 2 -> write x Out_channel.stderr
  | S x, H (_, Out ch) -> write x ch
  | _ -> type_err "write"

and op_seek ctxs =              (* seek *)
  let x = Rt.pop_eval ctxs in
  let h = Rt.pop () in
  match x, h with
  | Z x, Z 0 -> In_channel.(seek stdin @ Int.to_int64 x)
  | Z x, Z 1 -> Out_channel.(seek stdout @ Int.to_int64 x)
  | Z x, Z 2 -> Out_channel.(seek stderr @ Int.to_int64 x)
  | Z x, H (_, Out ch) -> Out_channel.seek ch @ Int.to_int64 x;
  | Z x, H (_, In ch) -> In_channel.seek ch @ Int.to_int64 x;
  | _ -> type_err "seek"

and op_close _ =                (* close *)
  match Rt.pop () with
  | Z 0 | Z 1 | Z 2 -> ()
  | H (_, Out ch) -> Out_channel.close ch
  | H (_, In ch) -> In_channel.close ch
  | _ -> type_err "close"

and op_eval ctxs =              (* eval *)
  match Rt.pop () with
  | S x -> Rt.eval ctxs @ Parser.parse x
  | _ -> type_err "eval"

and op_include ctxs =           (* include *)
  match Rt.pop () with
  | S x -> Rt.eval ctxs @ Parser.parse @ In_channel.read_all x
  | _ -> type_err "include"

and op_rand _ =              (* rand *)
  Rt.push @
    match Rt.pop () with
    | Z 0 -> R (Random.float 1.)
    | Z n -> Z (Random.int n)
    | _ -> type_err "rand"

and op_lower _ =                (* lower *)
  match Rt.pop () with
  | S x -> Rt.push @ S (String.lowercase x)
  | _ -> type_err "lower"

and op_upper _ =                (* upper *)
  match Rt.pop () with
  | S x -> Rt.push @ S (String.uppercase x)
  | _ -> type_err "upper"

let builtin =
  [("+",        true,   op_add);
   ("-",        true,   op_sub);
   ("*",        true,   op_mul);
   ("%",        true,   op_div);
   ("mod",      true,   op_mod);
   ("**",       true,   op_pow);
   (".",        true,   op_apply);
   ("/",        true,   op_fold);
   ("\\",       true,   op_scan);
   ("?",        true,   op_find);
   ("'",        true,   op_map);
   ("''",       true,   op_map2);
   ("~",        true,   op_set);
   (":",        true,   op_set2);
   ("::",       true,   op_reassign);
   ("==",       true,   op_eq);
   ("&&",       true,   op_and);
   ("||",       true,   op_or);
   ("=",        true,   op_trues);
   ("<",        true,   op_lt);
   (">",        true,   op_gt);
   ("gq",       true,   op_geq);
   ("lq",       true,   op_leq);
   (",",        true,   op_concat);
   (",,",       true,   op_cons);
   ("#",        true,   op_take);
   ("_",        true,   op_drop);
   ("sv",       true,   op_sv);
   ("vs",       true,   op_vs);
   ("enlist",   true,   op_enlist);
   ("of",       true,   op_cast);
   ("@",        true,   op_get);
   ("$",        true,   op_swap_apply);
   ("in",       true,   op_in);
   ("inter",    true,   op_inter);
   ("union",    true,   op_union);
   ("cut",      true,   op_cut);
   ("fix",      true,   op_fix);
   ("fixes",    true,   op_fixes);
   ("cmp",      true,   op_cmp);
   ("do",       true,   op_do);
   ("sort",     true,   op_sort);
   ("open",     true,   op_open);
   ("read",     true,   op_read);
   ("write",    true,   op_write);
   ("writel",   true,   op_writel);
   ("readl",    false,  op_readl);
   ("close",    false,  op_close);
   ("^",        false,  op_delist);
   ("cat",      false,  op_cat);
   ("cats",     false,  op_cats);
   ("every",    false,  op_every);
   ("any",      false,  op_any);
   ("uniq",     false,  op_uniq);
   ("min",      false,  op_min);
   ("max",      false,  op_max);
   ("asc",      false,  op_asc);
   ("dsc",      false,  op_dsc);
   ("flip",     false,  op_flip);
   ("sum",      false,  op_sum);
   ("prod",     false,  op_prod);
   ("sums",     false,  op_sums);
   ("prods",    false,  op_prods);
   ("where",    false,  op_where);
   ("ln",       false,  op_ln);
   ("sin",      false,  op_sin);
   ("cos",      false,  op_cos);
   ("tan",      false,  op_tan);
   ("floor",    false,  op_floor);
   ("abs",      false,  op_abs);
   ("ceil",     false,  op_ceil);
   ("if",       false,  op_if);
   ("cond",     false,  op_cond);
   ("]",        false,  op_list_end);
   ("[",        false,  op_list_begin);
   ("neg",      false,  op_neg);
   ("rev",      false,  op_rev);
   ("dup",      false,  op_dup);
   ("drop",     false,  op_drop_stk);
   ("swap",     false,  op_swap);
   ("til",      false,  op_til);
   ("len",      false,  op_len);
   ("type",     false,  op_type);
   ("measure",  false,  op_measure);
   ("eval",     false,  op_eval);
   ("include",  false,  op_include);
   ("rand",     false,  op_rand);
   ("lower",    false,  op_lower);
   ("upper",    false,  op_upper)
  ]
