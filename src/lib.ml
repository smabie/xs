open Core
open Defs

let rec with_list ctxs f = op_list_end ctxs; f (); op_list_begin ctxs
and with_list_rev ctxs f = op_list_end ctxs; f (); op_list_begin_rev ctxs

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
          Rt.push x; Rt.push ys.(ix)
        ) else (
          Rt.push ys.(ix); Rt.push x;
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
      | L qs -> Array.iteri qs (fun ix q -> Rt.bind ctxs q (Rt.get ix))
      | _ -> type_err ":")
  | N -> print_endline @ xs_to_string (Rt.peek ())
  | _ ->  type_err ":"

and op_set2 ctxs =              (* :: *)
  match Rt.pop () with
  | Q _ as q -> Rt.bind ctxs q (Rt.pop ())
  | F _ as f ->
     Rt.call_fn f ctxs;
     (match Rt.pop () with
      | L qs -> Array.iter qs (fun q -> Rt.bind ctxs q (Rt.pop ()))
      | _ -> type_err "::")
  | N -> print_endline @ xs_to_string (Rt.pop ())
  | _ ->  type_err "::"

and op_apply ctxs =             (* . *)
  match Rt.pop_get ctxs with
  | F _ as f -> Rt.call_fn f (Rt.create_ctx () :: ctxs)
  | _ -> type_err "."

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
         Rt.call_fn f ctxs;
         go (ix + 1)
       ) in
     Rt.push xs.(0);
     go 1
  | F _ as f, x ->
     let rec go prev =
       Rt.call_fn f ctxs;
       let y = Rt.peek () in
       if xs_eq x y || xs_eq prev y then ()
       else go y in
     Rt.push x;
     Rt.call_fn f ctxs;
     let y = Rt.peek () in
     if xs_eq y x then ()
     else go y
  | _ -> type_err "/"

and op_scan ctxs =              (* \ *)
  let f = Rt.pop_get ctxs in
  let x = Rt.pop () in
  match f, x with
  | F _ as f, L xs ->
     let ys =
       Array.init (Array.length xs)
         (fun ix ->
           if ix = 0 then (
             Rt.push xs.(ix);
             xs.(ix)
           ) else (
             Rt.push xs.(ix);
             Rt.swap 0 1;
             Rt.call_fn f ctxs;
             Rt.peek ()
           )
         ) in
     Rt.push @ L ys
  | F _ as f, x ->
     with_list_rev ctxs @
       fun () ->
       let rec go prev =
         Rt.dup ();
         Rt.call_fn f ctxs;
         let y = Rt.peek () in
         if xs_eq x y || xs_eq prev y then
           let _ = Rt.pop () in  ()
         else
           go y in
       Rt.push x;
       Rt.dup ();
       Rt.call_fn f ctxs;
       let y = Rt.peek () in
       if xs_eq y x then
         let _ = Rt.pop () in ()
       else go y
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
and op_swap _ = Rt.swap 0 1  (* swap *)

and op_til _ =                  (* til *)
  match Rt.pop () with
  | Z x -> Rt.push @ L (Array.init x (fun ix -> Z ix))
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
           | _ -> type_err "cond"
         ) in
       go 0
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
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  Rt.push @
    match x, y with
    | Z ix, L ys -> ys.(ix mod Array.length ys)
    | L ixs, L ys ->
       L (Array.map ixs
            (function
             | Z ix when ix < Array.length ys -> ys.(ix)
             | Z _ -> N
             | _ -> type_err "@"))
    | Z ix, S x -> S (Char.to_string @ String.get x ix)
    | L ixs, S x ->
       L (Array.map ixs
            (function
             | Z ix when ix < String.length x ->
                S (Char.to_string @ String.get x ix)
             | Z _ -> N
             | _ -> type_err "@"))
    | _ -> type_err "@"

and op_find ctxs =              (* ? *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  let find_ix x xs =
    Z (match Array.findi xs (fun _ y -> xs_eq x y) with
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
  | _ -> type_err ""

and op_sum _ =               (* sum *)
  match Rt.pop () with
  | L xs ->
     Rt.push @
       Z (Array.fold xs ~init:0
            ~f:(fun x y ->
              match x, y with
              | x, Z y -> x + y
              | _ -> type_err "sum"))
  | _ -> type_err "sum"

and op_prod _ =               (* prod *)
  match Rt.pop () with
  | L xs ->
     Rt.push @
       Z (Array.fold xs ~init:1
            ~f:(fun x y ->
              match x, y with
              | x, Z y -> x * y
              | _ -> type_err "prod"))
  | _ -> type_err "prod"

and op_where _ =                (* where *)
  match Rt.pop () with
  | L xs ->
     let n =
       Array.fold xs ~init:0
         ~f:(fun x y ->
           match x, y with
           | x, Z y -> x + y
           | x, B y -> x + Bool.to_int y
           | _ -> type_err "where") in
     let ys = Array.create n N in
     let rec go iy ix =
       if ix = Array.length xs then L ys
       else (
         let x =
           match xs.(ix) with
           | Z x -> x
           | B x -> Bool.to_int x
           | _ -> type_err "where" in
         for i = iy to iy + x - 1 do
           ys.(i) <- Z ix
         done;
         go (iy + x) (ix + 1)
       ) in
     Rt.push @ go 0 0
  | _ -> type_err "where"

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

and op_enlist ctxs =            (* enlist *)
  match Rt.pop_eval ctxs with
  | Z n -> Rt.push @ L (Array.init n (fun _ -> Rt.pop ()))
  | _ -> type_err "enlist"

and op_read _ =                 (* read *)
  match Rt.pop () with
  | S x ->
     In_channel.read_lines ~fix_win_eol:true x |>
       Array.of_list_map ~f:(fun x -> S x) |>
       fun x -> Rt.push @ L x
  | _ -> type_err "read"

and op_write _ =                (* write *)
  let x = Rt.pop () in
  let y = Rt.pop () in
  match x, y with
  | S x, L ys ->
     Array.to_list ys |>
       List.map ~f:(function | S x -> x | _ -> type_err "write") |>
       Out_channel.write_lines x
  | _ -> type_err "write"

and op_measure ctxs =           (* measure *)
  match Rt.pop () with
  | F _ as f ->
     let t = Unix.gettimeofday () in
     Rt.call_fn f ctxs;
     Rt.push @ R (Unix.gettimeofday () -. t)
  | _ -> type_err "measure"

and op_sv ctxs =                (* sv *)
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
              | _ -> type_err "sv"))
  | _ -> type_err "sv"

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
    | Q ("Z" | "z"), S x -> Z (int_of_string x)
    | Q ("Z" | "z"), R x -> Z (int_of_float x)
    | Q ("R" | "r"), S x -> R (float_of_string x)
    | Q ("R" | "r"), Z x -> R (float_of_int x)
    | Q ("Q" | "q"), S x -> Q x
    | Q ("S" | "s"), Q x -> S x
    | Q ("S" | "s"), Z x -> S (Int.to_string x)
    | Q ("S" | "s"), R x -> S (Float.to_string x)
    | Q ("L" | "l"), S x ->
       L (Array.map (String.to_array x)
            ~f:(fun x -> S (Char.to_string x)))
    | _ -> type_err "$"

and op_in ctxs =                   (* in *)
  let x = Rt.pop_eval ctxs in
  let y = Rt.pop () in
  let exists x xs = Array.exists xs ~f:(xs_eq x) in
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
       L (Array.filter xs
            (fun x -> Array.exists ys ~f:(xs_eq x)))
    | S x, S y ->
       S (String.filter x (fun c -> String.contains y c))
    | _ -> type_err "inter"

and op_swap_apply ctxs =        (* $ *)
  match Rt.pop_get ctxs with
  | F _ as f ->
     Rt.swap 0 1;
     Rt.call_fn f ctxs;
  | _ -> type_err "$"
let builtin =
  [("+",        true,   op_add);
   ("-",        true,   op_sub);
   ("*",        true,   op_mul);
   ("%",        true,   op_div);
   ("**",       true,   op_pow);
   (".",        true,   op_apply);
   ("/",        true,   op_fold);
   ("\\",       true,   op_scan);
   ("?",        true,   op_find);
   ("'",        true,   op_map);
   ("''",       true,   op_map2);
   ("~",        true,   op_set);
   (":",        true,   op_set2);
   ("==",       true,   op_eq);
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
   ("sum",      false,  op_sum);
   ("prod",     false,  op_prod);
   ("where",    false,  op_where);
   ("ln",       false,  op_ln);
   ("sin",      false,  op_sin);
   ("cos",      false,  op_cos);
   ("tan",      false,  op_tan);
   ("floor",    false,  op_floor);
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
   ("read",     false,  op_read);
   ("write",    false,  op_write);
   ("measure",  false,  op_measure)]
