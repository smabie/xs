/* lib.re */
/* 
 * This file is public domain as declared by Sturm Mabie
 */

open Defs

open Context
open Core
  
let ctx = Ctx.create();

/* name, is_oper, fn */
[("+", true, ctxs => {
  let x = Stk.pop_get(ctxs);
  let y = Stk.pop();
  
  Stk.push $
    switch ((x, y)) {
    | (Z(x), Z(y)) => Z(x + y)
    | (Z(x), R(y)) => R(float_of_int(x) +. y)
    | (R(x), Z(y)) => R(x +. float_of_int(y))
    | (R(x), R(y)) => R(x +. y)
    | _ => type_err("+")
    }
}), ("-", true, ctxs => {
  let x = Stk.pop_get(ctxs);
  let y = Stk.pop();
  
  Stk.push $ switch (x, y) {
    | (Z(x), Z(y)) => Z(x - y)
    | (Z(x), R(y)) => R(float_of_int(x) -. y)
    | (R(x), Z(y)) => R(x -. float_of_int(y))
    | (R(x), R(y)) => R(x -. y)
    | _ => type_err("-")
  }
}), ("*", true, ctxs => {
  let x = Stk.pop_get(ctxs);
  let y = Stk.pop();

  Stk.push $ switch ((x, y)) {
    | (Z(x), Z(y)) => Z(x * y)
    | (Z(x), R(y)) => R(float_of_int(x) *. y)
    | (R(x), Z(y)) => R(x *. float_of_int(y))
    | (R(x), R(y)) => R(x *. y)
    | _ => type_err("*")
}
}),
 ("%", true, ctxs => {
  let x = Stk.pop_get(ctxs);
  let y = Stk.pop();
  
  Stk.push $ switch (x, y) {
    | (Z(x), Z(y)) => Z(x / y)
    | (Z(x), R(y)) => R(float_of_int(x) /. y)
    | (R(x), Z(y)) => R(x /. float_of_int(y))
    | (R(x), R(y)) => R(x /. y)
    | _ => raise $ Failure("+ applied on invalid types")
  }
}), ("neg", false, ctxs => {
  Stk.push $ switch (Stk.pop()) {
    | Z(x) => Z(-1 * x)
    | R(x) => R(-1.0 *. x)
    | _ => type_err("neg")
  }
}), (":", true, ctxs => {
  let q = Stk.pop();
  let v = Stk.pop();

  switch ((q, v)) {
  | (Q(x), y) =>
    switch (List.hd(ctxs)) {
    | Some(ctx) => Ctx.bind(ctx, x, y)
    | None => raise $ Failure ("no context found")
    }
  | _ => type_err(":")
  }
}), (".", true, ctxs => {
  switch (Stk.pop_get(ctxs)) {
  | F({is_oper:_, instrs:Either.Second(f)}) => f(ctxs)
  | F({is_oper:_, instrs:Either.First(xs)}) => ()
  | _ => type_err(".")
  }
}), ("]", false, _ => Stk.push(N)),
 //("[", false' )
] |>
  List.iter(_, fun | (s, is_oper, f) => Ctx.bind(ctx, s) $ XsFn.create_builtin(is_oper, f));
