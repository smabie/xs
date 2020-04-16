/* context.re */
/* 
 * This file is public domain as declared by Sturm Mabie
 */

open Defs
open Core
open Res

module XsFn = {
  let create_builtin = (~is_oper, f) =>
    F({is_oper:is_oper, instrs:Either.Second(f)});
    
  let create = (~is_oper, xs) =>
    F({is_oper:is_oper, instrs:Either.First(Array.of_list(xs))});
};

module Ctx = {
  /* make a new empty context */
  let create = () => Hashtbl.create(module String);

  /* bind a new identifier to the context */
  let bind = (t, k: string, v: xs_val) => Hashtbl.set(t, k, v);

  /* 
   * Returns true if the string is an operator, false if the symbol is not an
   * operator or if the definition does not exist. Used to determine if we should
   * swap before evaluating.
   */
  let rec is_oper = (ts, k) => {
    switch (ts) {
    | [t, ...ts] =>
      switch(Hashtbl.find(t, k)) {
      | Some(F({is_oper:x, instrs:_})) => x
      | Some(_) => false
      | None => is_oper(ts, k)
      }
    | [] => false
    }
  };
  
  let rec lookup = (ts, k) =>
    switch ((ts, k)) {
    | ([t, ...ts], k) =>
      switch(Hashtbl.find(t, k)) {
      | Some(x) => x
      | None => lookup(ts, k)
      }
    | ([], _) => raise $ Failure(Printf.sprintf("%s not found", k))
    }
};

