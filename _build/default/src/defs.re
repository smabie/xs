/* context.re */
/* 
 * This file is public domain as declared by Sturm Mabie
 */

open Core
open Res
  
let ($) = (@@);
let type_err = s => raise $ Failure(Printf.sprintf("%s applied on invalid types", s));

type xs_val =
  | Z(int)                      /* int      */
  | R(float)                    /* float    */
  | Q(string)                   /* quote    */
  | B(bool)                     /* bool     */
  | N                           /* null     */
  | S(string)                   /* string   */
  | F(fn_t)                     /* function */
  | L(Array.t(xs_val))          /* list */
and parse_val =
| Sep 
| Int(int)                    /* Z */
| Float(float)                /* R */
| Quote(string)               /* Q */
| Ident(string)
| Oper(string)
| Bool(bool)                  /* B */
| Null                        /* N */
| Str(string)                 /* S */
| Fn(list(parse_val))
| InfixFn(list(parse_val))
| Expr(list(parse_val))
and fn_t = {
  is_oper: bool,
  instrs: Either.t(Array.t(xs_val), list(Hashtbl.t(string, xs_val)) => unit)
};

let rec parse_print = x =>
  switch(x) {
  | Int(x)   => Printf.printf("%d ", x);
  | Expr(xs) => List.iter(xs, parse_print)
  | Oper(x)  => Printf.printf("%s ", x)
  | Quote(x) => Printf.printf("`%s ", x)
  | Null     => Printf.printf("0N ")
  | Fn(xs)   => List.iter(xs, parse_print)
  | InfixFn(xs) => List.iter(xs, parse_print)
  | Str(x)   => Printf.printf("`%s ", x)
  | Ident(x) => Printf.printf("%s ", x)
  | Bool(x)  => Printf.printf("%b ", x)
  | Float(x) => Printf.printf("%f ", x)
  | Sep      => print_endline("foo")
  };

let rec xs_print = x =>
  switch (x) {
  | Z(x) => Printf.printf("%d ", x)
  | R(x) => Printf.printf("%f ", x)
  | Q(x) => Printf.printf("`%s ", x)
  | B(x) => Printf.printf("%b ", x)
  | N    => Printf.printf("0N ")
  | S(x) => Printf.printf("\"%s\" ", x)
  | F(x) => print_endline("foo")
  | _ => ()
  };

/* Append list to array t in place */
let res_append = (t, xs) => List.iter(xs, _) $ (x) => Array.add_one(t, x)
    
