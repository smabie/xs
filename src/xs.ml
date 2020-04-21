
open Core
open Angstrom
open Res

open Defs
open Parser  

let rec repl () =
  let ctxs = [Rt.setup Lib.builtin] in
  let rec go () =
    print_string("xs> ");
    Rt.eval ctxs @ parse @ read_line ();
    Rt.display ();
    go () in
  go ()
;;

repl ()


