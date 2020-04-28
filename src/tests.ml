open Core
open Defs

let run s =
  let ctxs = [Rt.setup Lib.builtin] in
  Rt.eval ctxs @ Parser.parse s;
  xs_to_string @ Rt.pop ()

let t x y = String.equal (run x) y

let%test _ = t "1+1"                    "2"
let%test _ = t "a+a~1"                  "2"
let%test _ = t "(1)+1"                  "2"
let%test _ = t "([1 2])+[1 2]"          "[2 4]"
let%test _ = t "1+[1 2]"                "[2 3]"
let%test _ = t "([1 2])+1"                "[2 3]"

let%test _ = t "1-1"                    "0"
let%test _ = t "a-a~1"                  "0"
let%test _ = t "(1)-1"                  "0"
let%test _ = t "([2 3])-[1 2]"          "[1 1]"
let%test _ = t "2-[1 1]"                "[1 1]"
let%test _ = t "([1 2])-1"              "[0 1]"

let%test _ = t "2*1"                    "2"
let%test _ = t "a*a~2"                  "4"
let%test _ = t "(2)*1"                  "2"
let%test _ = t "([1 2])*[1 2]"          "[1 4]"
let%test _ = t "1*[1 2]"                "[1 2]"
let%test _ = t "([1 2])*3"              "[3 6]"

let%test _ = t "2%1"                    "2"
let%test _ = t "a%a~2"                  "1"
let%test _ = t "(2)%1"                  "2"
let%test _ = t "([4 2])%[1 2]"          "[4 1]"
let%test _ = t "4%[1 2]"                "[4 2]"
let%test _ = t "([3 6])%3"              "[1 2]"

let%test _ = t "(1+ 2)*3"               "9"

let%test _ = t "neg 5"                  "-5"

let%test _ = t "a+a~3"                  "6"
let%test _ = t "a*(a~3)*a+a~1"          "6"
let%test _ = t "([`a`b`c])~1 2 3"       "1"

let%test _ = t "a a~3"                 "3"
let%test _ = t "a([`a`b`c])~1 2 3"     "1"

let%test _ = t "+/[1 2 3 4]"            "10"
let%test _ = t "(+.)/[1 2 3 4]"         "10"
let%test _ = t "+\\[1 2 3]"             "[1 3 6]"
let%test _ = t "(x:; x%2)\\6"          "[6 3 1 0]"
let%test _ = t "(x:; x%2)/6"           "0"

let%test _ = t "(2+)'[1 2]"             "[3 4]"

let%test _ = t "(+.)''[1 2] [2 1]"      "[3 3]"

let%test _ = t "rev [2 1]"              "[1 2]"

let%test _ = t "+. dup 2"               "4"

let%test _ = t "drop 2 3"               "3"

let%test _ = t "^2 3"                   "3"

let%test _ = t "til 3"                  "[0 1 2]"

let%test _ = t "1=3"                    "0b"
let%test _ = t "1=1"                    "1b"
let%test _ = t "0n=0n"                  "1b"
let%test _ = t "\"a\"=\"a\""            "1b"
let%test _ = t "([1 2])==[1 2]"         "1b"
let%test _ = t "([1 2])==[0 2]"         "0b"
let%test _ = t "0b=1b"                  "0b"
let%test _ = t "(`a)=`a"                "1b"
let%test _ = t "((1))=(1)"              "1b"

let%test _ = t "if 0==0 (0) (1)"         "0"
let%test _ = t "if 0==1 (0) (1)"         "1"

let%test _ = t "([1 2])=[1 2]"          "[1b 1b]"
let%test _ = t "1=[1 2]"                "[1b 0b]"

let%test _ = t "([1 2]),[1 0]"          "[1 2 1 0]"
let%test _ = t "5,,[1 0]"               "[5 1 0]"

let%test _ = t "len [1 2 3]"            "3"

let%test _ = t "cond [(1b) (1) (2)]"    "1"
let%test _ = t "cond [(0b) (1) (2)]"    "2"

let%test _ = t "3#0"                    "[0 0 0]"
let%test _ = t "3#[1 2 3 4]"            "[1 2 3]"
let%test _ = t "(neg 2)#[1 2 3 4]"      "[3 4]"
let%test _ = t "3#[1 2]"                "[1 2 1]"

let%test _ = t "3 enlist 1 2 3"         "[1 2 3]"

let%test _ = t "\",\" sv [\"a\" \"b\"]" "\"a,b\""

let%test _ = t "\",\" vs \"a,b\""       "[\"a\" \"b\"]"

let%test _ = t "2?[5 2 7]"              "1"
let%test _ = t "([2 2 7])?[5 2 7]"      "[1 1 2]"

let%test _ = t "where [2 0 1]"          "[0 0 2]"

let%test _ = t "2_til 5"                "[2 3 4]"
let%test _ = t "(neg 2)_til 5"          "[0 1 2]"

let%test _ = t "sum til 4"              "6"
let%test _ = t "prod [1 2 3]"           "6"

