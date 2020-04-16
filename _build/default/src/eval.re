/* eval.re */
/* 
 * This file is public domain as declared by Sturm Mabie
 */
  
open Defs
open Context

open Core
open Res


let swap_opers = (ctxs, xs) => {
  let swap = (idx) => {
    Array.swap(xs, idx, idx + 1);
    switch(Array.get(xs, idx)) {
    | Ident(x) | Oper(x) => Array.set(xs, idx, Quote(x))
    | _ => ()
    }
  };
    
  let idx = ref(0);
  while (idx^ < Array.length(xs)) {
    switch (Array.get(xs, idx^)) {
    | Ident(x) | Oper(x) =>
      if (Ctx.is_oper(ctxs, x)) {
        swap(idx^);
        idx := 1 + idx^
      }
    | _ => ()
    };
    idx := 1 + idx^
  };
  xs
}

/* toplevel eval  */
let rec eval = (ctxs, x) =>
  switch (x) {
  | Int(x) => Stk.push(Z(x))
  | Float(x) => Stk.push(R(x))
  | Bool(x) => Stk.push(B(x))
  | Null => Stk.push(N)
  | Str(x) => Stk.push(S(x))
  | Quote(x) => Stk.push(Q(x))
  | Expr(xs) =>
    Array.of_list(xs) |> swap_opers(ctxs, _) |> Array.iter(eval(ctxs), _);
  | Oper(x) | Ident(x) =>
    switch (Ctx.lookup(ctxs, x)) {
    | F({is_oper:_, instrs:Either.Second(f)}) => f(ctxs)
    | F({is_oper:_, instrs:Either.First(xs)}) => ()
    | x => Stk.push(x)
    }
  | _ => ()
};

