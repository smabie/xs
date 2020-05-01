(* open Core
 * open Defs
 *
 * module RArray = Res.Array
 *
 * let push_z x ctxs = Rt.push (Z x)
 * let push_f x ctxs = Rt.push (R x)
 * let push_b x ctxs = Rt.push (B x)
 * let push_n ctxs = Rt.push N
 * let push_s x ctxs = Rt.push (S x)
 * let push_q x ctxs = Rt.push (Q x)
 * let push_ident x ctxs =
 *   match Rt.lookup ctxs x with
 *   | F2 _ as f -> Rt.call_fn2 f ctxs
 *   | x -> Rt.push x
 *
 * let push_fn f ctxs = Rt.push f
 *
 * let exec_vm xs ctxs =
 *   let len = RArray.length xs in
 *   let rec go idx =
 *     if idx = len - 1 then ()
 *     else (
 *       (RArray.get xs idx) ctxs;
 *       go (idx + 1)
 *     ) in
 *   go 0
 *
 *
 * let rec compile x =
 *   let make_fn ~is_oper xs =
 *     let instrs = Either.First (RArray.map compile (RArray.of_list xs)) in
 *     F2 { is_oper; instrs } in
 *   match x with
 *   | Int x -> push_z x
 *   | Float x -> push_f x
 *   | Bool x -> push_b x
 *   | Null -> push_n
 *   | Str x -> push_s x
 *   | Quote x -> push_q x
 *   | Ident x -> push_ident x
 *   | Expr xs -> exec_vm (RArray.map compile (RArray.of_list xs))
 *   | Fn xs -> push_fn @ make_fn ~is_oper:false xs
 *   | InfixFn xs -> push_fn @ make_fn ~is_oper:true xs
 *   | _ -> push_z 5 *)
