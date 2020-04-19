(* context.re *)
(* 
 * This file is public domain as declared by Sturm Mabie
 *)


open Core
open Res

open Defs

module XsFn = struct
  let create_builtin ~is_oper  f = F { is_oper; instrs = Either.Second f }
         
  let create ~is_oper xs = F { is_oper; instrs = Either.First xs }
end

module Ctx = struct
  let create () = Hashtbl.create (module String)
  let bind t (k : string) (v : xs_val) = Hashtbl.set t k v
  
  let rec is_oper ts k =
    match ts with
    | t::ts ->
       (match Hashtbl.find t k with
        | Some F { is_oper = x; instrs = _ }  -> x
        | Some _ -> false
        | None -> is_oper ts k)
    | [] -> false
  
  let rec lookup ts k =
    match ts, k with
    | t::ts, k ->
       (match Hashtbl.find t k with
        | Some x -> x
        | None -> lookup ts k)
    | [], _ -> raise @ Failure (sprintf "%s not found" k)

  let setup xs =
    let ctx = create () in
    List.iter xs
      (function | s, is_oper, f -> bind ctx s @ XsFn.create_builtin is_oper f);
    ctx

end
