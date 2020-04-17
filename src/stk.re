/* stack.re */
/* 
 * This file is public domain as declared by Sturm Mabie
 */

open Defs
open Core
open Res
open Context
  
let stk: Array.t(xs_val) = Array.empty();

let convert = (idx) => Array.length(stk) - idx - 1;
let get = (idx) => Array.get(stk, convert(idx));
let push = (v) => Array.add_one(stk, v);
let pop = () => {
  let v = get(0);
  Array.remove_one(stk);
  v
};
let peek = () => get(0);

/* pop val, if a quoted name (Q) get the value, otherwise return */
let pop_get = ctxs => {
  switch (pop()) {
  | Q(x) => Ctx.lookup(ctxs, x)
  | x    => x
  }
};

let swap = (x, y) => Array.swap(stk, convert(x), convert(y));
let len = Array.length(stk);

let display = () => {
  Array.iteri((idx, x) => Printf.printf("%d: %s\n", idx, xs_print(x)), stk)
};

