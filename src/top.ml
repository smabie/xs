
open Core

open Defs
open Parser  

let repl () =
  let ctxs = [Rt.setup Lib.builtin] in
  let rec go () =
    Out_channel.output_string stdout "xs> ";
    Out_channel.flush stdout;
    Rt.eval ctxs @ parse @ In_channel.(input_line_exn stdin);
    Rt.display ();
    go () in
  go ()

let load path =
  let ctxs = [Rt.setup Lib.builtin] in
  Rt.eval ctxs @ parse @ In_channel.read_all path

