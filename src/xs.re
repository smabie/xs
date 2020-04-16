/* xs.re */
/* 
 * This file is public domain as declared by Sturm Mabie
 */

open Core
open Angstrom
open Res
  
open Parser  
open Defs
open Eval
open Stk
open Lib
open Context
  
eval([ctx]) $ parse("3**. 3 2");

Array.to_list(Stk.stk) |> List.iter(_, fun | Z(x) => print_int(x) | _ => ())


