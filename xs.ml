open Core
open Xslib.Defs

let filename_param =
  let open Command.Param in
  anon ("filename" %: string)

let command =
  Command.basic
    ~summary:"Public domain xs interpreter"
    ~readme:(fun () ->
{|This is the main binary for the xs language. For more information information 
check out the project's github page at http://github.com/smabie/xs|}
    )
    Command.Let_syntax.(
    let%map_open fname = anon (maybe ("filename" %: string)) in
    fun () ->
    match fname with
    | Some f -> Xslib.Top.load f
    | None ->
       print_endline
{|Public domain interpreter for the xs language. Created by Sturm Mabie
(sturm@cryptm.org). For more information check out the project's github at
http://github.com/smabie/xs|};
       Xslib.Top.repl ())

let () =
  Command.run ~version:"0.1" ~build_info:"" command

