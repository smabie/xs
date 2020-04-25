
open Core

open Defs
open Parser  

let repl () =
  let ctxs = [Rt.setup Lib.builtin] in
  let rec go () =
    print_string("xs> ");
    Rt.eval ctxs @ parse @ read_line ();
    Rt.display ();
    go () in
  go ()

let load path =
  let ctxs = [Rt.setup Lib.builtin] in
  Rt.eval ctxs @ parse @ In_channel.read_all path
