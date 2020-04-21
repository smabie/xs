open Core
open Defs

let ctxs = [Rt.setup Lib.builtin]

let run s =
  Rt.eval ctxs @ Parser.parse s;
  xs_print @ Rt.pop ()

let t x y = String.equal (run x) y

let%test _ = t "1+1"                    "2"
let%test _ = t "a+a:1"                  "2"
let%test _ = t "(1)+1"                  "2"
let%test _ = t "([1 2])+[1 2]"          "[2 4]"
let%test _ = t "1+[1 2]"                "[2 3]"
let%test _ = t "([1 2])+1"                "[2 3]"

let%test _ = t "1-1"                    "0"
let%test _ = t "a-a:1"                  "0"
let%test _ = t "(1)-1"                  "0"
let%test _ = t "([2 3])-[1 2]"          "[1 1]"
let%test _ = t "2-[1 1]"                "[1 1]"
let%test _ = t "([1 2])-1"              "[0 1]"

let%test _ = t "2*1"                    "2"
let%test _ = t "a*a:2"                  "4"
let%test _ = t "(2)*1"                  "2"
let%test _ = t "([1 2])*[1 2]"          "[1 4]"
let%test _ = t "1*[1 2]"                "[1 2]"
let%test _ = t "([1 2])*3"              "[3 6]"

let%test _ = t "2%1"                    "2"
let%test _ = t "a%a:2"                  "1"
let%test _ = t "(2)%1"                  "2"
let%test _ = t "([4 2])%[1 2]"          "[4 1]"
let%test _ = t "4%[1 2]"                "[4 2]"
let%test _ = t "([3 6])%3"              "[1 2]"

let%test _ = t "(1+ 2)*3"               "9"

let%test _ = t "neg 5"                  "-5"

let%test _ = t "a+a:3"                  "6"
let%test _ = t "a*(a:3)*a+a:1"          "6"

let%test _ = t "+/[1 2 3 4]"            "10"
let%test _ = t "(+.)/[1 2 3 4]"         "10"

let%test _ = t "(2+)'[1 2]"             "[3 4]"

let%test _ = t "(+.)''[1 2] [2 1]"      "[3 3]"

let%test _ = t "rev [2 1]"              "[1 2]"

let%test _ = t "dup 2"                  "2"

