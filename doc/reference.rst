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
   booleans = {"0" | "1"}, "b";
   null = "0n" | "0N";
   string = "\"", {char}, "\"";
   fn = "(", {expr}, ")";
   infix_fn = "{", {expr}, "}";
   expr = {{ws}, (float | integer | identifier | quote | boolean | null | string | fn | infix_fn)};

A valid identifier is either a letter followed by one or more letters
and numbers or a symbol (``oper`` in the grammar) repeated one or more
times, such as ``%%%`` or ``$``. Note that one is not allowed to
define an operator such as ``%#`` as each of these symbols will be
parsed seperately. ``[``, ``]``, and ``.`` will always be parsed
individually, even if not seperated by whitespace.

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

Though not a distinct data-type (though it might be in the future),
there's special syntactic sugar for boolean lists: ::

  xs> type 1011b
  0: `L

```1011b`` is equivalent to ``[1b 0b 1b 1b]``.

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
current running context instead of the lexical context: ::

  xs> f:(x)
  xs> (x:5; f).
  0: 5

Take care to not inadvertantly access variables defined in the
caller's scope.

Math
####

``neg`` unary negative
-----------------------
::

   neg {Z | R | L} -> {Z | R | L}

Unary negative of a value, broadcasts across lists.

``+`` addition
---------------
::

  {Z | R | L} + {Z | R | L} -> {Z | R | L}

Add two values together. Broadcasts across lists.

``-`` substraction
-------------------
::

  {Z | R | L} - {Z | R | L} -> {Z | R | L}

Subtract two values. Broadcasts across lists.

``*`` multiplication
---------------------
::

  {Z | R | L} * {Z | R | L} -> {Z | R | L}

Multiply two values. Broadcasts across lists.

``%`` division
---------------

::

  {Z | R | L} % {Z | R | L} -> {Z | R | L}

Divide two values. Broadcasts across lists.

``mod`` modulus
----------------
::

  {Z | R | L} mod {Z | R | L} -> {Z | R | L}

Remainder of two values. Broadcasts across lists.

``**`` power
--------------
::

  {Z | R | L} ** {Z | R | L} -> {Z | R | L}

Power of two values. Broadcasts across lists.

``ln`` natural logarithm
-------------------------
::

  ln {Z | R | L} -> {Z | R | L}

Natural logarithm, broadcasts across lists.

``sin`` sine
-------------
::

  sin {Z | R | L} -> {Z | R | L}

Sin, broadcasts across lists.

``cos`` cosine
---------------
::

  cos {Z | R | L} -> {Z | R | L}

Cosine, broadcasts across lists.

``tan`` tangent
----------------
::

  tan {Z | R | L} -> {Z | R | L}

Tangent, broadcasts across lists.

``sum`` sum of list
--------------------
::

   sum L -> {Z | R}

Finds the sum of a list: ::

  xs> sum til 5
  0: 10

``sums`` partial sums of list
------------------------------
::

   sums L -> L

Returns the partial sums of the list: ::

  xs> sums til 5
  0: [0 1 3 6 10]

``prod`` product of list
-------------------------
::

   prod L -> {Z | R}

Find the product of all elements in a list: ::

   xs> prod 1+til 4
   0: 24

``prods`` partial products of list
-----------------------------------
::

   prods L -> L

Returns the partial sums of the list: ::

   xs> prods til 1+til 4
   0: [1 2 6 24]

``abs`` absolute value
-----------------------
::

   abs {Z | R | L} -> {Z | R | L}

Find the absolute value of a number or list: ::

   xs> abs (til 10) - 10
   0: [10 9 8 7 6 5 4 3 2 1]


``ceil`` ceiling
-----------------
::

   ceil {Z | R | L} -> {Z | R | L}

Find the ceiling of a number or list: ::

  xs> ceil 3.2
  0: 4

``floor`` floor
----------------
::

   floor {Z | R | L} -> {Z | R | L}

Find the floor of a number or list: ::

  xs> floor 3.2
  0: 3

Boolean and Conditionals
########################

``==`` equals
--------------
::

   x == y -> B

Test equality between two values.

``<`` less than
----------------
::

   x < y -> B

Return ``1b`` if the first argument is less than the second, otherwise
``0b``.

``>`` greater than
-------------------
::

   x > y -> B

Returns ``1b`` if the first argument is greater than the second,
otherwise ``0b``.

``gq`` greater or equal
------------------------
::

   x gq y -> B

Returns ``1b`` if the first argument is greater or equal to the
second, otherwise ``0b``.

``lq`` less or equal
---------------------
::

   x lq y -> B

Return ``1b`` if the first argument is less than or equal to the
second, otherwise ``0b``.

``&&`` And
-----------
::

   B && B -> B

Logical and operation.


``||`` Or
----------
::

   B || B -> B

Logical or.


``if`` if expression
---------------------
::

   if cond:B f:F g:F -> {f. | g.}

Evaluate ``f`` if ``cond`` is ``1b``, otherwise evaluates. ``g``.

``cond`` multiple conditional
------------------------------
::

   cond [(cond_a: F) (f: F) (cond_b: F) (g: F) ... (h: F)] -> {f. | g. | h. ...}

Take a list of functions and tests each condition in order, executing
the corresponding function if the condition is true. Note that only
one function is ever executed and that the length of the list given to
``cond`` must be odd. ::

  xs> x:3; cond [(x==0) ("hello") (x==2) ("goodbye") ("world")]
  0: "world"

``every`` tests all true
-------------------------
::

   every {L | B} -> B

Return true if the given boolean is true or if the given list only
contains ``1b``.

``any`` any true
-----------------
::

   any {L | B} -> B

Return true if the given boolean is true or if the given list
contains at least one ``1b``.

``cmp`` comparison
-------------------
::

   x:* cmp y:* -> {-1 | 0 | 1}

Return ``-1`` if ``x`` is less than ``y``, ``0`` if ``x`` equals
``y``, and ``1`` if ``x`` is greater than ``y``.

Stack Manipulation
##################

``dup`` duplicate element
--------------------------
::

   dup x:* -> x x

Duplicate the top element of the stack.

``swap`` swap elements
-----------------------
::

   swap x:* y:* -> y x

Swap ``x`` and ``y`` on the Stack.

``drop`` drop value
--------------------
::

   drop A -> ()

Discard the top element of the stack.


Function Application
####################

``.`` apply
------------
::

   x:* . -> x.

If ``x`` is a symbol, look up the value and call it if a function,
push onto the stack otherwise. If ``x`` is a function literal, call
``x``, if ``x`` is any other value, simply push ``x`` onto the stack.

``$`` swap and apply
---------------------
::

   f $ x y -> f. y x

Swap ``x`` and ``y`` and then apply ``f``.

Assignment
##########

``:`` set/print
---------------
::

   {Q | L} : x... -> ()

Take a symbol or a list of symbols and bind them to the corresponding
values on the stack: ::

  xs> x:5; x;
  0: 5

  xs> ([`x`y]):3 5; x y;
  1: 5
  0: 3

If given a null instead of a symbol, ``:`` will print out the value to
``stdout``: ::

  xs> 0n:"Hello, World!"
  "Hello, World!"

``~`` peek set/print
--------------------
::

   {Q | L} : x... -> x...

``~`` binds a variable or multiple variables to the local context much
like ``:``, except the value is not popped off the stack: ::

  xs> x+x~5
  0: 10

  xs> ([`x`y])~2 3; x+y+ +.
  0: 10

Likewise, if given a null, ``~`` will print out the value(s) to
``stdout``: ::

  xs> 0n~5
  5
  0: 5

``::`` reassign
---------------
::

   {Q | L} :: x... -> ()

Resassign a variable that already has a definition. Allows the user to
modify variables outside the local scope: ::

  xs> x:5; (x::3); x
  0: 3

Iterators and Accumulators
##########################

``'`` map
---------
::

   f:F ' xs:L -> L

Apply ``f`` to each element of ``xs``: ::

  xs> (2*)'til 3
  0: [0 2 4]

``''`` map2
-----------
::

   f:F '' xs:L ys:L -> L

Apply ``f`` to each pair of values from ``xs`` and ``ys``. The length
of ``xs`` and ``ys`` must be equal. Example: ::

  xs> (+.)''til 5 til 5
  0: [0 2 4 6 8]

``/`` fold
----------
::

   f:F / xs:L -> *

Fold ``f`` over ``xs``: ::

  xs> +/til 4
  0: 10

``\`` scan
----------
::

   f:F \ xs:L -> *

Fold ``f`` over ``xs``, keeping each partial fold: ::

  xs> +\til 4
  0:[0 1 3 6]

``fix`` fixpoint
----------------
::

   fix f:F x:*-> *

Successively apply ``f`` on ``x`` until two sucessive values equal
each other or one value equals the starting value ``x``: ::

  xs>(x:; x%2) fix 6
  0: 0

``fixes`` partial fixpoints
---------------------------
::

   fixes f:F x:* -> L

Successively apply ``f`` on ``x`` until two sucessive values equal
each other or one value equals the starting value ``x`` while keeping
intermediate values: ::

  xs> (x:; x%2) fixes 6
  0: [6 3 1 0]

``do`` iteration
----------------
::

   x:{F | Z} do f:F -> *

Apply the function ``f`` either ``x`` times if ``x`` is an integer; if
a function, until ``x`` returns ``0b``: ::

  xs> 3 do ("hello")
  2: "hello"
  1: "hello"
  0: "hello"

List  and String
################

``[]`` make list
----------------

``[]`` are special functions that can be used to create a new list: ::

  xs> [[1 2] [3 4]]
  0: [[1 2] [3 4]]

``enlist`` make list
--------------------
::

   n:Z enlist x... -> L

Create a list from the top ``n`` elements from the stack: ::

  xs> 3 enlist 1 2 3
  0:[1 2 3]

``^`` delist
------------
::

   ^ L -> x...

Convert a list into elements on the stack: ::

  xs> ^[1 2 3]
  2: 3
  1: 2
  0: 1

``til`` construct numbered list
-------------------------------
::

   til n:Z -> L

Construct a list between ``0`` and ``n`` (exclusive): ::

  xs> til 4
  [0 1 2 3]

``len`` length of list
----------------------
::

   len xs:{L | S} -> Z

Return the length of ``xs``: ::

  xs> len til 3
  0: 3

  xs> len "abc"
  0: 3

``flip`` flip list
------------------
::

   flip {L | S} -> L

Flip a multidimensional list across its first two dimensions. In the
case of a list with only one dimension (or a string), creates a column
vector: ::

  xs> flip [[1 2] [3 4] [5 6]]
  0: [[1 3 5] [2 4 6]]

  xs> flip til 3
  0: [[0] [1] [2]]

  xs> flip ["abc" "abc" "abc"]
  0: ["aaa" "bbb" "ccc"]

  xs> flip "abc"
  0: ["a" "b" "c"]

``rev`` reverse
---------------
::

   rev {L | S} -> {L | S}

Reverse the given list or string: ::

  xs> rev til 3
  0:[2 1 0]

  xs> rev "abc"
  0:"cba"

``,`` concatenate
-----------------
::

   x:* | y:* -> L

Concatenate two values. If both ``x`` and ``y`` are lists, append
them; if both are atoms, create a list with two elements; if one is a
list and one an atom, cons the value onto the front or back of the
list: ::

  xs> 3,4
  0:[3 4]

  xs> 3,[1 2]
  0: [3 1 2]

  xs> ([1 2]),3
  0: [1 2 3]

  xs> ([1 2]),[3 4]
  0: [1 2 3 4]

  xs> "foo","bar"
  0: "foobar"

``,,`` cons
-----------
::

   x:* ,, xs:L -> L
   xs:L ,, x:* -> L

places ``x`` at the head or end of ``xs``: ::

  xs> 3,,[1 2]
  0: [3 1 2]

  xs> ([3]),,[1 2]
  0: [[3] 1 2]

  xs> ([1 2]),,3
  0: [1 2 3]

Indexing, Reshaping, Changing
#############################

``@`` get
---------
::

   {L | Z} @ {L| S} -> *

Take an index or list of indices and return either the single element
stored at that index or a list containing the elements at the given
indices, modulo the length of the list: ::

  xs> 2@til 3
  0: 2

  xs> 4@[1 2]
  0: 1

  xs> ([1 3])@[5 2 7]
  0: [2 5]

``?`` find
----------
::

   {* | L | S} ? {L | S} -> {* | L}

Find the element(s) in the list or string and return the corresponding
indices: ::

  xs> 3?[5 3 2]
  0: 1

  xs> ([3 2])?[5 3 2]
  0: [1 2]

  xs> "hll"?"hello"
  0: [0 2 2]

If the element cannot be found, ``?`` returns the length of the list: ::

  xs> 10?til 4
  0: 4

Note that ``?`` always returns the index of the first found element.

``where``
---------
::

   where x:L -> L

Return a list containing, for each item of ``x``, that number of
copies of its index: ::

  xs> where [1 2 3]
  0: [0 1 1 2 2 2]

  xs> where [0b 1b 0b 1b]
  0: [1 3]

Where is often used in conjunction with ``@`` to select out certain
elements from a list. For example, if we wanted to get all even
numbers between 0 and 10: ::

  xs> (where (0==mod$ 2)'x)@x~til 10
  0: [0 2 4 6 8]

``#`` take
----------
::

   n:Z # xs:{L | S |  *} -> {L | S}


Take ``n`` elements from ``xs``. If ``n`` is positive, take from the
front of the ``xs``, if negative, take from the back: ::

  xs> 2#til 5
  0: [0 1]

  xs> (neg 2)#til 5
  0: [3 4]

If ``n`` is greater than the length of ``xs``, ``#`` wraps around as
needed: ::

  xs> 5#til 3
  0: [0 1 2 0 1]

If ``xs`` is not a list or string, create a list containing ``n``
copies of ``xs``: ::

  xs> 5#3
  0: [3 3 3 3 3]

``_`` drop
----------
::

   n:Z _ xs:{L | S} -> {L | S}

Drop ``n`` elements from ``xs``. If ``n`` is positive, drop from the
front of the list, otherwise drop from the end: ::

  xs> 2_til 5
  0: [2 3 4]

  xs> (neg 2)_til 5
  0: [0 1 2]

  xs> 6_til 5
  0: []


``cut`` reshape
---------------
::

   x:{Z | L} cut y:{L | S} -> L

If ``x`` is a integer, cut every ``x`` element: ::

  xs> 3 cut til 10
  0: [[0 1 2] [3 4 5] [6 7 8] [9]]

  xs> 2 cut "abcd"
  0: ["ab" "cd"]

If ``x`` is a list, cut at the indices: ::

  xs> ([2 4]) cut til 10
  0: [[2 3] [4 5 6 7 8 9]]

  xs> ([2 4]) cut "abcdefg"
  0: ["cd" "efg"]

``cat`` concatenate all
-----------------------
::

   cat xs:L -> {L | S}

Fold ``,`` over ``xs``: ::

  xs> cat ["a" "b" "c"]
  0: "abc"

  xs> cat [1 2 3]
  0: [1 2 3]

  xs> cat [[1 2] [3 4]]
  0: [1 2 3 4]

``cats`` partial concatenate all
--------------------------------
::

   cats xs:L -> L

Scan ``.`` over ``xs``: ::

  xs> cats [[1 2] [3 4]]
  0: [[1 2] [1 2 3 4]]

  xs> cats ["a" "b" "c"]
  0: ["a" "ab" "abc"]

Set Operations
##############

``in`` contains
---------------
::

   x:{L | *} in L -> xs:{L | B}

Returns a boolean or list of booleans indicating whether the
element(s) were found in ``xs``: ::

  xs> 5 in til 10
  0: 1b

  xs> ([5 10 2]) in til 10
  0: 101b

  xs> "ac" in "abc"
  0: 11b

``inter`` intersection
----------------------
::

   x:{L | S} inter y:{L | S} -> {L | S}

Find the intersection of ``x`` and ``y``: ::

  xs> "ac" inter "ab"
  0: "a"

  xs> ([2 5 3]) inter [2 4 3]
  0: [2 3]

``union``
---------
::

   x:{L | S | *} union y:{L | S} -> {L | S}

Find the union of ``x`` and ``y``: ::

  xs> 3 union til 2
  0: [0 1 3]

  xs> ([1 2]) union [3 4]
  0: [1 2 3 4]

  xs> "ab" union "ac"
  0: "abc"

``uniq`` unique elements
------------------------
::

   uniq {L | S | *} -> {L | S}

Return a list of unique elements. If given an atom instead of a string
or a list, return a list containing that atom: ::

  xs> uniq 3
  0: [3]

  xs> uniq [1 2 1 3]
  0: [1 2 3]

  xs> uniq "aabccc"
  0: "abc"

Sorting
#######

``asc`` sort ascending
----------------------
::

   asc x:{L | S | *} -> {L | S}

Sort ``x`` from smallest to largest. If ``x`` is an atom, return a
list with just one element: ::

  xs> asc 4
  0: [4]

  xs> asc rev til 4
  0:[0 1 2 3]

  xs> asc "csa"
  0: "acs"

``dsc`` sort descending
-----------------------
::

   dsc x:{L | S | *} -> {L | S}

Sort ``x`` from largest to smallest. If ``x`` is an atom, return a
list with just one element: ::

  xs> dsc 4
  0: [4]

  xs> dsc til 4
  0: [3 2 1 0]

  xs> dsc "csa"
  0: "sca"

``sort`` custom sort
--------------------
::

   f:F sort {L | S} -> {L | S}

Sort using comparison function ``f``. ``f`` must return either -1, 0,
or 1 depending on whether the first argument is smaller than, equal
to, or greater than the second argument. ``cmp sort`` is identical to
``asc``: ::

  xs> cmp sort [3 1 2]
  0: [1 2 3]

  xs> cmp sort "bca"
  0: "abc"

  xs> ((neg 1)*cmp.) sort [3 1 2]
  0: [3 2 1]

Types
#####

``of`` type conversion
----------------------
::

   q:Q of x:* -> *

Convert ``x`` to the type specified by ``q``. For a list of valid
``q`` values, Check out the list of data types at the beginning of
this reference manual. Examples: ::

  xs> `R of 5
  0: 5.00000

  xs> `L of "abc"
  0: ["a" "b" "c"]

  xs> `S of `hello
  0: "hello"

``type``
--------
::

   type x:* -> Q

Return the type of ``x``: ::

  xs> type 5.2
  0: `F

  xs> type "ab"
  0: `S

  xs> type til 5
  0: `L

``sv`` scalar from vector
-------------------------
::

   x:S sv xs:L -> S

Take a string and a list of strings and return a string from the
concatenation of ``xs`` interspersed with ``x``: ::

  xs> ", " sv ["hello" "world"]
  0: "hello, world"

*Todo*: in the future, this function will do other scalar from vector
 operations as well.

``vs`` vector from scalar
-------------------------
::

   x:S vs y:S -> L

Break up the list ``y`` into a list, using ``x`` as a seperator: ::

  xs> ", " vs "hello, world"
  0: ["hello" "world"]

*Todo*: in the future, this function will do other vector from scalar
 operations as well.

I/O
###

``open`` open file
------------------
::

   {`r | `w | `a} open f:S -> H

Open a file specified by the filepath ``f`` in read (```r``), write
(```w``), or append (```a``) mode and return a handle to tha file.


``read`` read data
------------------
::

   n:Z read {H | 0} -> S

Read ``n`` characters from either stdin (0), or a file handle that was
opened in read mode.

``write`` write data
--------------------
::

   x:S write {H | 1 | 2} -> ()

Write ``x`` to either stdout (1), stderr (2), or the given file
handle.


``seek`` change file position
-----------------------------
::

   i:Z seek {0 | 1 | 2 | H} -> ()

Move to position ``i`` of the given handle. 0 Represents stdin, 1
stdout, and 2 stderr.

``close`` close file handle
---------------------------
::

   close H -> ()

Close the given file handle.

``readl`` read lines
--------------------
::

   readl f:S -> L

Read the entire file at the filepath ``f`` as a list of strings, one
string per line.

``writel`` write lines
----------------------
::

   xs:{S | L} writel f:{1 | 2 | S} -> ()

Write the list of strings ``xs`` to the file at filepath ``f``, stdout
(``1``), or stderr ``2``.

Misc
####

``rand`` random value
---------------------
::

   rand x:0 -> R
   rand x:Z -> Z

If ``x`` is 0, return a real number between 0 and 1
inclusive. Otherwise, return a number between 0 and ``x`` exclusive: ::

  xs> rand 0
  0: 0.06343

  xs> rand 2
  0: 1

``include`` evaluate file
-------------------------
::

   include f:S

Evaluate the file referenced by filepath ``f`` in the current context.

``measure`` elapsed time
------------------------
::

   measure f:F -> R

Measure the time taken to execute function ``f``.

``eval`` evaluate
-----------------
::

   eval x:S -> *...

Evaluate the given string ``x`` in the current scope.
