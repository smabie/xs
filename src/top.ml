open Core

open Defs
open Parser

(* read lines until parens/brackets are balanced *)
let read () =
  let count s =
    String.fold s ~init:0
      ~f:(fun n c ->
        match c with
        | '{' | '(' | '[' -> n + 1
        | '}' | ')' | ']' -> n - 1
        | _ -> n) in
  let buf = Buffer.create 160 in
  let rec go n  =
    match In_channel.(input_line_exn stdin) with
    | exception _ -> exit 0
    | s ->
       Buffer.add_string buf s;
       let c = n + count s in
       if c = 0 then
         Buffer.contents buf
       else if c < 0 then
         failwith "Invalid syntax, not parsing"
       else go c in
  go 0

let repl () =
  let ctxs = [Rt.setup Lib.builtin] in
  let rec go () =
    Out_channel.output_string stdout "xs> ";
    Out_channel.flush stdout;
    match Rt.eval ctxs @ parse @ read () with
    | exception End_of_file -> exit 0
    | exception exn ->
       print_endline @ Exn.to_string exn;
       Res.Array.clear Rt.stk;
       Stack.clear Rt.xstk;
       go ()
    | () ->
       Rt.display ();
       go () in
  go ()

let load path =
  let ctxs = [Rt.setup Lib.builtin] in
  Rt.eval ctxs @ parse @ In_channel.read_all path
