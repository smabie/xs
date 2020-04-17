/* parser.re */
/* 
 * This file is public domain as declared by Sturm Mabie
 */
open Defs
open Core
open Angstrom
open Res

/* list of special operators, each is parsed individually */
let opers = String.to_list("+=!@#$%^&*-_/?[]~<>,.:");

let is_whitespace = fun | ' ' | '\t' | '\n' | '\r' => true | _ => false;
let is_digit = fun | '0'..'9' => true | _ => false;
let is_alpha = fun | 'a'..'z' | 'A'..'Z' => true | _ => false;
let is_oper = c => List.mem(opers, c, (===));

/* whitespace */
let ws = take_while(is_whitespace);
let sep = char(';') >>| _ => Sep;
  
/* floating point (Float) or integer (Int) value */
let number = {
  let dot = peek_char >>= fun
  | Some('.') => advance(1) >>| () => true
  | _ => return(false);
  
  take_while1(is_digit) >>= digits => dot >>= fun
  | false => return $ Int(int_of_string(digits))
  | true  => take_while1(is_digit) >>| frac => Float(float_of_string(digits ++ "." ++ frac))
};

/* identifiers */
let ident = take_while1(is_alpha) >>= alphas =>
  take_while(is_digit) >>| digits => Ident(alphas ++ digits);

/* same as an identifier, but preceded by a backtick, used for variable names */
let quote = char('`') *> take_while1(is_alpha) >>= alphas =>
  take_while(is_digit) >>| digits => Quote(alphas ++ digits);

/* 
 * single character operators. They have no special semantics so we just generate
 * the Ident type instead of them having their own type
 */
let oper = choice(List.map(opers, char)) >>| op => Ident(Char.to_string(op));

/* 0b -> false, 1b - true */
let boolean = string_ci("1b") <|> string_ci("0b") >>| b =>
  switch(String.lowercase(b)) {
  | "0b" => Bool(false)
  | "1b" => Bool(true)
  | _    => assert(false) /* not reached */
  };

/* 0n -> null */
let null = string_ci("0n") *> return(Null);

/* a string, can span multiple lines */
let str = char('\"') *> take_while(c => c !== '\"') <* char('\"') >>| s => Str(s);

/* An expression */
let expr = fix(expr => {
  let fn = char('(') *> expr <* char(')') >>| x => Fn(x);
  let infix_fn = char('{') *> expr <* char('}') >>| x => InfixFn(x);
  let xs = [str, quote, sep, null, boolean, number, oper, ident, fn, infix_fn];
  many $ ws *> choice(xs) <* ws
}) >>= parsed =>
  peek_char >>= fun
  | Some(c) => fail $ Format.sprintf("Parsing failed on '%c'", c)
  | None    => return $ Expr(parsed);

/* Create sub-expressions from seperators */
let rec mk_tree = expr => {
  let go = xs => {
    List.group(xs, (_, x) => x === Sep)
    |> List.map(_, ys => List.rev $ List.filter(ys, x => x !== Sep))
    |> List.concat
    |> List.map(_, mk_tree) 
  };
      
  switch(expr) {
  | Expr(xs)    => Expr(go(xs))
  | Fn(xs)      => Fn(go(xs))
  | InfixFn(xs) => InfixFn(go(xs))
  | x => x
  }
}

/* top-level parse */
let parse = s =>
  parse_string(expr, s) |> fun
  | Ok(xs)    => mk_tree(xs)
  | Error(s)  => raise $ Sys_error(s);
