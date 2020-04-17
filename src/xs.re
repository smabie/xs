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
  
let rec repl = () => {
  print_string("xs> ");
  eval([ctx]) $ parse(read_line());
  Stk.display();
  repl();
};

repl();
