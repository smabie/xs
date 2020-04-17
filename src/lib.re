/* lib.re */
/* 
 * This file is public domain as declared by Sturm Mabie
 */

open Defs

open Context
open Core
open Res
  
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
    | _ => type_err("%")
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
 ("[", false, ctxs => {
  let xs = Array.empty();

  let rec go = () => {
    switch (Stk.pop()) {
    | N => Stk.push(L(xs))
    | x =>
      Array.add_one(xs, x);
      go()
    }
  };
  go()
}), ("/", true, ctxs => {
  let f = Stk.pop_get(ctxs);
  let x = Stk.pop();

  switch ((f, x)) {
  | (F({is_oper:_, instrs:Either.Second(f)}), L(xs)) =>
    let fn = (b, a) => {
      switch ((b, a)) {
      | (N, y) => y
      | (x, y) =>
        Stk.push(y);
        Stk.push(x);
        f(ctxs);
        Stk.pop()
      }
    };
    
    Stk.push $ Array.fold_left(fn, N, xs)
  | (F({is_oper:_, instrs:Either.First(ys)}), L(xs)) => ()
  | _ => type_err("/")
  }
})

] |>
  List.iter(_, fun | (s, is_oper, f) => Ctx.bind(ctx, s) $ XsFn.create_builtin(is_oper, f));
