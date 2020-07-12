(* parser.ml *)
(*
 * This file is public domain as declared by Sturm Mabie
 *)

open Core
open Angstrom

open Defs

(* operators that are parsed individually *)
let sopers = String.to_list "[]."

(* Operators that confirm to the regex opers+ ex: ++, %%%, etc *)
let opers = String.to_list "|+=!@#$%^&*-_\\/?~<>,:'"

let is_whitespace = function | ' '|'\t'|'\n'|'\r' -> true | _ -> false
let is_digit = function | '0'..'9' -> true | _ -> false
let is_alpha = function | 'a'..'z'|'A'..'Z' -> true | _ -> false

let ws = take_while is_whitespace
let sep = (char ';') >>| (fun _ -> Sep)

(* floating point (Float) or integer (Int) value *)
let number =
  let dot =
    peek_char >>=
      function
       | Some '.' -> advance 1 >>| fun () -> true
       | _ -> return false in
  take_while1 is_digit >>=
    fun digits ->
    dot >>= function
    | false -> return @ Int (int_of_string digits)
    | true ->
       take_while1 is_digit >>|
         fun frac -> Float (float_of_string @ digits ^ "." ^ frac)

(* identifiers *)
let ident =
  take_while1 is_alpha >>=
    fun alphas -> take_while is_digit >>|
    fun digits -> Ident (alphas ^ digits)

(* same as an identifier, but preceded by a backtick, used for variable names *)
let quote =
  char '`' *> take_while1 is_alpha >>=
    fun alphas -> take_while is_digit >>|
    fun digits -> Quote (alphas ^ digits)

let comment = char '!' *> many(not_char '\n') >>| fun _ -> Sep

(*
 * single character operators. They have no special semantics so we just generate
 * the Ident type instead of them having their own type
 *)
let soper = choice (List.map sopers char) >>| fun op -> Ident (Char.to_string op)

let oper =
  choice @ List.map opers (fun x -> many1 (char x)) >>|
    fun cs -> Ident (String.of_char_list cs)

(* A singe boolean value or a boolean array. ex: 1b, 101b *)
let booleans =
  many1 (char '1' <|> char '0') <* (char 'b' <|> char 'B') >>| fun xs ->
  match xs with
  | ['1'] -> Bool true
  | ['0'] -> Bool false
  | xs ->
     BoolList (Array.of_list_map xs
                 (function
                  | '1' -> true
                  | '0' -> false
                  | _ -> false))

(* 0n -> null *)
let null = string_ci "0n" *> return Null

(* a string, can span multiple lines *)
let str =
  char '"' *>
    take_while (fun c -> not @ Char.equal c '"') <* char '"' >>| fun s -> Str s

(* An expression, currently only used for the top level value *)
let expr =
  fix (fun expr ->
      let fn = char '(' *> expr <* char ')' >>| fun x -> Fn x in
      let infix_fn = char '{' *> expr <* char '}' >>| fun x -> InfixFn x in
      let xs = [comment; str; quote; sep; null; booleans;
                number; oper; soper; ident; fn; infix_fn] in
      (many @ ws *> choice xs) <* ws >>| List.to_array) >>=
    fun parsed ->
    peek_char >>= function
    | Some c -> fail @ sprintf "Parsing failed on '%c'" c
    | None -> return @ Expr parsed

(* Create sub-expressions from seperators *)
let rec mk_tree expr =
  let go xs =
    List.group xs (fun _ x -> phys_equal x Sep) |>
      List.map
        ~f:(fun ys -> List.rev @ List.filter ys (fun x -> not @ phys_equal x Sep)) |>
      List.concat |>
      (fun x -> List.map x mk_tree) |>
      List.to_array in
  match expr with
  | Expr xs -> Expr (go @ Array.to_list xs)
  | Fn xs -> Fn (go @ Array.to_list xs)
  | InfixFn xs -> InfixFn (go @ Array.to_list xs)
  | x -> x

(* top-level parse *)
let parse s =
  match parse_string Consume.All expr s with
  | Ok xs -> mk_tree xs
  | Error s -> failwith s
