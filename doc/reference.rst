Language Reference
==================

Command-Line Parameters
#######################

*xs* can either be called with no arguments to start up the REPL, or
passed one argument: the path of the script to execute.

Syntax
######

Below is the EBNF grammar for *xs*:
::
   char = ..
   digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
   letter = "A" | "B" | "C" | "D" | "E" | "F" | "G"
       | "H" | "I" | "J" | "K" | "L" | "M" | "N"
       | "O" | "P" | "Q" | "R" | "S" | "T" | "U"
       | "V" | "W" | "X" | "Y" | "Z" | "a" | "b"
       | "c" | "d" | "e" | "f" | "g" | "h" | "i"
       | "j" | "k" | "l" | "m" | "n" | "o" | "p"
       | "q" | "r" | "s" | "t" | "u" | "v" | "w"
       | "x" | "y" | "z" ;
   ws = " " | "\t" | "\n" | "\r";

   oper = one_or_more_of "|+=!@#$%^&*-_\\/?~<>,:'";
   individual_oper = ("[" | "]" | ".");
   integer = {digit};
   float = integer, ".", integer;
   identifier = {letter}, {digit};
   quote = "`",  identifier;
   boolean = "0b" | "1b";
   null = "0n" | "0N";
   string = "\"", {char}, "\"";
   fn = "(", {expr}, ")";
   infix_fn = "{", {expr}, "}";
   expr = {{ws}, (float | integer | identifier | quote | boolean | null | string | fn | infix_fn)};

A valid identifier is either a letter followed by one or more letters
and numbers or a symbol repeated one or more times, such as ``%%%`` or
``$``. Note that one is not allowed to defined an operator such as
``%#`` as each of these symbols will be parsed seperately. ``[``,
``]``, and ``.`` will always be parsed seperately, even if not
seperated by whitespace.


Data Types
##########

+--------+----------------------------+---------------------+
| symbol | description                | example             |
+========+============================+=====================+
| \`Z    | 61-bit integer type        | 42                  |
+--------+----------------------------+---------------------+
| \`R    | 64-bit floating point type | 1.235               |
+--------+----------------------------+---------------------+
| \`B    | Boolean type               | 1b                  |
+--------+----------------------------+---------------------+
| \`Q    | Symbol type                | `hello              |
+--------+----------------------------+---------------------+
| \`S    | String type                | "world"             |
+--------+----------------------------+---------------------+
| \`F    | Function type              | (3*2+)              |
+--------+----------------------------+---------------------+
| \`L    | List type                  | [1 2 3]             |
+--------+----------------------------+---------------------+
| \`H    | I/O handle                 | h:open `R "foo.txt" |
+--------+----------------------------+---------------------+
| \`N    | Null type                  | 0n                  |
+--------+----------------------------+---------------------+

Parsing
#######

*xs* is parsed top to bottom, left to right. Each line or expression
is seperated by a semicolon. The following shows the equivalency
between various expressions: ::

   f; g; h -> h g f
   f g; h -> g f h
   a b; c d; -> b a d c

Scoping
#######

*xs* is dynamically scoped instead of lexically scoped. This means
that an evaluated identifier will look for its definition in the
current running context instead of the lexical context. Take care to
not inadvertantly access variables defined in the caller's scope.

Math
####

``neg``: Unary Negative
------------------------
::

   neg {Z | R | L}

Unary negative of a value, broadcasts across lists.

``+``: Addition
----------------
::

  {Z | R | L} + {Z | R | L}

Adds two values together. Broadcasts across lists.

``-``: Substraction
--------------------
::

  {Z | R | L} - {Z | R | L}

Subtract two values. Broadcasts across lists.

``*``: Multiplication
----------------------
::

  {Z | R | L} * {Z | R | L}

Multiplies two values. Broadcasts across lists.

``%``: Division
----------------

::

  {Z | R | L} % {Z | R | L}

Divides two values. Broadcasts across lists.

``mod``: Modulus
-------------
::

  {Z | R | L} mod {Z | R | L}

Remainder of two values. Broadcasts across lists.

``**``: Power
--------------
::

  {Z | R | L} ** {Z | R | L}

Power of two values. Broadcasts across lists.

``ln``: Natural logarithm
--------------
::

  ln {Z | R | L}

Natural logarithm, broadcasts across lists.

``sin``: Sine
--------------
::

  sin {Z | R | L}

Sin, broadcasts across lists.

``cos``: Cosine
----------------
::

  cos {Z | R | L}

Cosine, broadcasts across lists.

``tan``: Tangent
-----------------
::

  tan {Z | R | L}

Tangent, broadcasts across lists.

``sum``: Sum of list
---------------------
::

   sum L

Finds the sum of a list: ::

  xs> sum til 5
  0: 10

``sums``: Partial sums of list
------------------------------
::

   sums L

Returns the partial sums of the list: ::

  xs> sums til 5
  0: [0 1 3 6 10]

``prod``: Product of list
--------------------------
::

   prod L

Find the product of all elements in a list: ::

   xs> prod 1+til 4
   0: 24

``prods``: Partial products of list
------------------------------------
::

   prods L

``abs``: Absolute value
------------------------
::

   abs {Z | R | L}

Find the absolute value of a number or list: ::

   xs> prods til 1+til 4
   0: [1 2 6 24]

``ceil``: Ceiling
-----------------------------------------------
::

   ceil {Z | R | L}

Find the ceiling of a number or list: ::

  xs> ceil 3.2
  0: 4

``floor``: Floor
-------------------------------------
::

   floor {Z | R | L}

Find the floor of a number or list: ::

  xs> floor 3.2
  0: 3

Boolean and Conditionals
########################

``==``: Equals
--------------
::

   A == A

Tests equality between two values.

``<``: Less than
----------------
::

   A < A

Returns ``1b`` if the first argument is less than the second,
otherwise ``0b``.

``>``: Greater than
-------------------
::

   A > A

Returns ``1b`` if the first argument is greater than the second,
otherwise ``0b``.

``gq``: Greater or equal
------------------------
::

   A > A

Returns ``1b`` if the first argument is greater or equal to the
second, otherwise ``0b``.

``lq``: Less or equal
---------------------
::

   x > y

Returns ``1b`` if the first argument is less than or equal to the
second, otherwise ``0b``.

``&&``: And
-----------
::

   B && B

Logical and operation.


``||``: Or
----------
::

   B || B

Logical or.


``if``: If expression
---------------------
::

   if (cond: B) (f: F) (g: F)

Evalues ``f`` if ``cond`` is ``1b``, otherwise evaluates. ``g``.

``cond``: Multiple conditional
------------------------------
::

   cond [(cond_a: F) (f: F) (cond_b: F) (g: F) .. (h: F)]

Takes a list of functions and tests each condition in order, executing
the corresponding function if the condition is true. Note that only
one function is ever executed and that the length of the list given to
``cond`` must be odd. ::

  xs> x:3; cond [(x==0) ("hello") (x==2) ("goodbye") ("world")]
  0: "world"

``every``: Tests all true
-------------------------
::

   every (L | B)

Returns true if the given boolean is true or if the given list only
contains ``1b``.

``any``: Any true
-----------------
::

   any (L | B)

Returns true if the given boolean is true or if the given list
contains at least one ``1b``.

``cmp``: Comparison
-------------------
::

   (x: A) cmp  (y: A)

Returns ``-1`` if ``x`` is less than ``y``, ``0`` if ``x`` equals
``y``, and ``1`` if ``x`` is greater than ``y``.

Stack Manipulation
##################

``dup``: Duplicate element
--------------------------
::

   dup A

Duplicate the top element of the stack.

``swap``: Swap elements
-----------------------
::

   swap (x: A) (y: A)

Swap ``x`` and ``y`` on the Stack.

``drop``; Drop value
----------------------
::

   drop A

Discard the top element of the stack.


Function Application
####################

``.``: Apply
------------
::

   x:A .

If ``x`` is a symbol, look up the value and call it if a function,
push onto the stack otherwise. If ``x`` is a function literal, call
``x``, if ``x`` is any other value, simply push ``x`` onto the stack.
