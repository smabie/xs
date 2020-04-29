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
let%test _ = t "rev \"123\""            "\"321\""

let%test _ = t "+. dup 2"               "4"

let%test _ = t "drop 2 3"               "3"

let%test _ = t "swap 2 3"               "3"

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
let%test _ = t "3#\"ab\""               "\"aba\""
let%test _ = t "0#\"a\""                "\"\""
let%test _ = t "(neg 2)#\"abc\""        "\"bc\""

let%test _ = t "3 enlist 1 2 3"         "[1 2 3]"

let%test _ = t "\",\" sv [\"a\" \"b\"]" "\"a,b\""

let%test _ = t "\",\" vs \"a,b\""       "[\"a\" \"b\"]"

let%test _ = t "2?[5 2 7]"              "1"
let%test _ = t "([2 2 7])?[5 2 7]"      "[1 1 2]"

let%test _ = t "where [2 0 1]"          "[0 0 2]"

let%test _ = t "2_til 5"                "[2 3 4]"
let%test _ = t "(neg 2)_til 5"          "[0 1 2]"
let%test _ = t "4_til 2"                "[]"
let%test _ = t "2_\"abc\""              "\"c\""
let%test _ = t "(neg 2)_\"abc\""        "\"a\""

let%test _ = t "sum til 4"              "6"
let%test _ = t "prod [1 2 3]"           "6"
let%test _ = t "sums til 3"             "[0 1 3]"
let%test _ = t "prods [1 2 3]"          "[1 2 6]"

let%test _ = t "5 in [1 2 4]"           "0b"
let%test _ = t "5 in [1 5 4]"           "1b"
let%test _ = t "([1 2]) in [1 3 4]"     "[1b 0b]"
let%test _ = t "\"ab\" in \"ac\""       "[1b 0b]"

let%test _ = t "([1 5]) inter [1 3 5]"  "[1 5]"
let%test _ = t "\"ab\" inter \"ac\""    "\"a\""

let%test _ = t "%$ 2 4"                 "2"

let%test _ = t "type 1"                 "`Z"
let%test _ = t "type 1.0"               "`R"
let%test _ = t "type til 2"             "`L"
let%test _ = t "type 1b"                "`B"
let%test _ = t "type `a"                "`Q"
let%test _ = t "type ()"                "`F"
let%test _ = t "type 0n"                "`N"
let%test _ = t "type \"\""              "`S"
let%test _ = t "2 cut til 4"            "[[0 1] [2 3]]"
let%test _ = t "([2 3])cut til 4"       "[[2] [3]]"
let%test _ = t "2 cut \"abc\""          "[\"ab\" \"c\"]"
let%test _ = t "([2 3])cut \"abcd\""    "[\"c\" \"d\"]"

let%test _ = t "flip [1 2]"             "[[1] [2]]"
let%test _ = t "flip [[1 2][3 4]]"      "[[1 3] [2 4]]"
let%test _ = t "flip \"\""              "[]"
let%test _ = t "flip [\"ab\" \"bc\"]"   "[\"ab\" \"bc\"]"
let%test _ = t "abs neg 3.2"            "3.200000"
let%test _ = t "abs neg 2"              "2"
let%test _ = t "abs [neg 2 neg 1]"      "[2 1]"
