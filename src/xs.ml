
open Core
open Angstrom
open Res
open In_channel

open Defs
open Context
open Parser  
open Stk
open Eval

let rec repl () =
  let ctxs = [Ctx.setup Lib.builtin] in
  let rec go () =
    print_string("xs> ");
    eval ctxs @ parse @ read_line ();
    Stk.display ();
    go () in
  go ()
;;

repl ()


