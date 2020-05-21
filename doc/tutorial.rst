Tutorial
========

Introduction
############

Welcome to *xs*, an functional concatenative array language inspired
by kdb+/q, forth, and others. The *xs* interpreter is written in OCaml
and released into the public domain by yours truly. *xs* might
initially strike you as unusual, strange, and perhaps even deliberatly
unfriendly (especially if you don't have any prior experience with
array languages); but I guarantee that in no time at all reading and
writing *xs* will become second nature!

If you haven't already, check out the installation instructions in
order to get a working environment. After the installation completes,
start the interpreter by simply running::

  $ xs

You should see a prompt similar to the below in which you can enter
commands::

  Public domain interpreter for the xs language. Created by Sturm Mabie
  (sturm@cryptm.org). For more information check out the project's github at
  http://github.com/smabie/xs
  xs>

Try adding two numbers::

  xs> 2+5
  0: 5

*xs* is a concatenative (or stack based) language in which all
operations are parsed from right-to-left.

You might be wondering at this point how the ``+`` operator works
since it's being used in an infix fashion. The answer is that *xs*
allows you to define special operators that switch the operator with
the position of it's left adjacent value. So internally, the
interpreter is evaluating ``+ 2 5``. Now clear the top most element:
::

  xs> drop
  xs>

From now on, we will assume that you are starting from a clean stack,
though you can keep old values on the stack if you so choose.

Since everything is evaluated from right-to-left, all operators have
the same precedence: ::

  xs> 2*3+1
  0: 8

In order to turn an operator into a normal function, we can use the
``.`` operator to apply the function: ::

  xs> +. 3 2
  0:5

You might be wondering how this works, since the interpreter will
switch the ``+`` with the ``.`` and then evaluate the ``.``. For
example if we just try: ::

  xs> + 3 2
  (Invalid_argument "index out of bounds")

We get an error. The interpreter is trying to switch the ``+``
operator with the value to its left, but there is no value to its
left, so we receive an error. One might think that the same thing
would happen with the ``.`` operator. In order to solve this problem,
*xs* will conditionally quote the left-most value if it is an
identifier. So ``+. 3 2`` will turn into: ::

  . `+ 3 2

A backtick followed by an identifier represents a quoted value, which
is in many ways similar to a string, but cannot contain spaces. What
follows is the sequence of operations taken to evaluate ``+. 3 2``:

1) The interpreter sees the integer ``2`` and pushes it onto the
   stack. The stack now looks like: ::

     0: 2

2) The interpreter sees the integer ``3`` and pushes it onto the
   stack. The stack now looks like: ::

     1: 2
     0: 3

3) The interpreter now sees the identifier ``.``. It looks up the
   identifier to see if it has a definition in the current scope. The
   definition is found and the identifior references an operator. Now
   *xs* looks at the value to the left of ``.``. It sees another
   identifier, which means it should push it's quoted form onto the
   stack: ::

     2: 2
     1: 3
     0: `+

4) Now *xs* goes back to the ``.`` and calls the function associated
   with its definition; in this case, the apply function. The apply
   function now expects one argument, an identifier or a function. If
   it's a function literal (a function that has been pushed onto the
   stack), it calls it. If it's an identifier it looks up the value
   (which must be a function), and calls it. Note that when a quoted
   value bound to a function is applied by ``.``, it does not matter
   if the function is an operator or not. Finally, the stack looks
   like: ::

     0: 5

Comments
########

Comments are denoted by the ``!`` symbol and span until the end of the
line: ::

  xs> 3+2       ! this is a comment
  0: 5

Function and Variables
######################

We could define a function that multiplies the top level value on the
stack by ``2`` with the following: ::

  xs> mul2:(2*)

We can now call it with ``5`` as the parameter: ::

  xs> mul2 5
  0: 10

Parenthesis are used to define a function and the ``:`` operator is
used to assign a value (including a function) to an identifier: ::

  xs> x:3
  xs> mul2 x
  0: 6

Note that we don't have to name the function in order to call it: ::

  xs> (2*). 5
  0: 10

If ``.`` is given a function it will be evaluated, if it's given an
identifier, it looks up the definition and pushes it onto the stack if
it's not a function; if it is, it will called as well. For example: ::

  xs> x:5
  xs> x.
  0:5

You can also use ``.`` on itself: ::

  xs> x:5
  xs> ..`x
  0:5

We need to use the ``.`` operator because when a function literal in
encountered, it is simply placed on the stack instead of being
evaluated. When bound to an identifier on the otherhand, the function
is automatically called. Note that we did not define any argument name
for the function, and instead used what is called point-free style. If
we wanted to explicitly bind the parameter to a name, we could write: ::

  xs> (x:; 2*x). 5
  0: 10

*xs* evalues expressions from from right-to-left, but lines can be
broken up with a semicolon. For example: ::

 xs> (2+2; 3+)
 0: 7

the ``:`` operator pops the value off of the stack, but we can use the
``~`` operator if we wish to only bind and leave the stack unchanged: ::

  xs> sq:(x*x~)
  xs> sq 5
  0: 25

This operator is often useful for intermediate or temporary variables
that are used multiple times in an expression.

Operators
#########

In *xs*, operators are not special and can be defined by the user. In
order to define an operator, we simply use curly brackets instead of
parenthesis to enclose our function definition: ::

  xs> add:{y:x; x+y}
  xs> 3 add 4
  0: 7

While this will work for literals, a problem arises when trying to
call ``add`` on a variable: ::

  xs> x:3; x add 4
  (Failure "+ applied on invalid types")

Behind the scenes, ``x`` is being passed in as the quoted literal
```x``, which is invalid. In order to solve this problem, we first
need to dereference ``x`` before passing the value to the ``+``
operator: ::

  xs> add:{y:x:..; x+y}
  xs> x:3; x add 4
  0: 7

The ``..`` isn't a unique operator, we're simply using the regular
``.`` operator on itself in order to use the prefix form of it.

Notice how in our first statement we bound the variable ``x`` to the
first-most value (after being dereferenced by ``.``) on the stack, and
then the variable ``y`` to the second-most value. This turns out to be
a common operation in *xs*, as the first line of many functions first
set up their variables (often times, using explicit variables can be
clearer than a point free style).  ``:`` and ``~`` therefore supports
binding multiple values at once: ::

  xs> ([`x`y`z]):1 2 3
  xs> x*y-z
  0: -1

What is happening here is that ``:`` is taking a function that
produces a list as its first argument and binding them, in order, to
values on the stack. The list syntax is unfamilar, but we will cover
that in the next section.

Like functions, operators are pushed onto the stack in literal form
and as such, are identical to regular functions until bound to an
identifier.

Lists
#####

The primary datatype in *xs* is the list datatype, represented
internal by an OCaml array. Creating a list is easy: ::

  xs> [1 2 3]
  0: [1 2 3]

Even though this looks like a list literal, both ``[`` and ``]`` are
actually functions defined in the language. ``]`` records the current
length of the data stack and pushes onto a special stack only used for
list construction. ``[`` then compares the length of the stack now to
the value pushed upon the list construction stack and creates a list
with a length equal to the difference between the two, using the
elements from the top of the stack to populate it.

Below are some operations we can do on lists: ::

  xs> ([1 2 3]),[4 5]           ! concatenate two lists
  0: [1 2 3 4 5]

  xs> ([0 3])@til 10            ! select the 0th and 3rd elements
  0: [0 2

  xs> rev til 3                 ! reverse
  0: [2 1 0]

  xs> flip [[1 2][3 4][5 6]]    ! flip rectangular list
  0: [[1 3 5] [2 4 6]]

  xs> 3?[1 3 5]                 ! return index of element
  0: 1

  xs> 3 enlist 1 2 3            ! make a list
  0: [1 2 3]

  xs> ^[1 2 3]                  ! push the list elements onto the stack
  2: 3
  1: 2
  0: 1

  xs> 4#0                       ! create a list of length 4 containing all 0s
  0: [0 0 0 0]

  xs> dsc til 5                 ! sort descending
  0: [4 3 2 1 0]

  xs> 2#til 10                  ! take first 2 elements
  0: [0 1]

  xs> (neg 2)#til 10            ! take last 2 elements
  0: [8 9]

  xs> 2_til 5                   ! drop first 2 elements
  0: [2 3 4]

  xs> (neg 2)_til 5             ! drop last 2 elements
  0: [0 1 2]

Most functions and operators in *xs* operate on lists as well as atoms: ::

  xs> 1+[1 2 3]
  0: [2 3 4]

If we wanted to have the list be the first argument instead of the
integer ``1``, we would need to surround it in a function: ::

  xs> ([1 2 3])+1
  0: [2 3 4]

This is because a list is only created after the ``[`` function is
executed, the ``+`` operator would not see the list as its first
argument, it would only see the ``3``. To solve this problem, A
function is passed instead, which is evaluated by ``+`` and result
added to ``1``. For this reason, all operators in *xs* can take a
function as their first argument.

To generate a list from 0 to 4 (exclusive), we can use the ``til``
function: ::

  xs> til 5
  0: [0 1 2 3 4]

If instead we wanted a list from 5 to 10, we can simply add 5 to it: ::

  xs>5+til 5
  0: [5 6 7 8 9]

Now what if we wanted the squares between 5 and 10?::

  xs>(x*x~)'5+til 5
  0: [25 36 49 64 81]

This introduces us to a new function, the ``'`` or map function. ``'``
simply applies the given function to each element of the list and
pushes the new list onto the stack. Now let's multiply them all
together: ::

  xs>*/(x*x~)'5+til 5
  0: 228614400

The ``/`` or fold function successively applies the given function to
the list until a single result is produced. For more information about
folds, click `here`_.

In order to see the intermediate results, we can use the ``\`` or scan
function: ::

  xs> *\(x*x~)'5+til 5
  0: [25 900 44100 2822400 228614400]

Note that scan uses the first element of the list as the first element
of the output list.

.. _here: https://en.wikipedia.org/wiki/Fold_(higher-order_function)


Scoping
#######

*xs* uses dynamic scoping which means that variables in a function
refer to the context in which they are called, not in the one that
they are written in: ::

  xs> f:(x)
  xs> (x:5; f).
  0: 5

While dynamic scoping is more flexible and powerful than lexical
scoping, care must be taken to not accidentally refer to an
incorrect variable in the surrounding scope.

Data Types
##########

Below is a table of data types in *xs*:

+--------+----------------------------+----------------------+
| symbol | description                | example              |
+========+============================+======================+
| \`Z    | 61-bit integer type        | 42                   |
+--------+----------------------------+----------------------+
| \`R    | 64-bit floating point type | 1.235                |
+--------+----------------------------+----------------------+
| \`B    | Boolean type               | 1b                   |
+--------+----------------------------+----------------------+
| \`Q    | Symbol type                | \`hello              |
+--------+----------------------------+----------------------+
| \`S    | String type                | "world"              |
+--------+----------------------------+----------------------+
| \`F    | Function type              | (3*2+)               |
+--------+----------------------------+----------------------+
| \`L    | List type                  | [1 2 3]              |
+--------+----------------------------+----------------------+
| \`H    | I/O handle                 | h:open \`R "foo.txt" |
+--------+----------------------------+----------------------+
| \`N    | Null type                  | 0n                   |
+--------+----------------------------+----------------------+

In order to convert from one data type to another, we can use the
``of`` operator: ::

  xs> `Z of 3.2
  0: 3
  xs> `s of 42
  0: "42"

Day 1 Advent of Code 2019 Solution
##################################

To conclude this quick tutorial, we're going to solve both parts of
the `Day 1 Advent of Code 2019`_ problem. Read the part 1 problem
first and then come back here.

First we need to read in the file of masses in order to calculate the
required fuel: ::

  masses:(`Z of)'readl "ex/1.txt";

The ``readl`` function reads the given file and returns a list of each
line of the file. In order to convert the strings to integers, we
first use the ``'`` (map) function to apply the conversion to each
element of the list. The `of` operator converts one datatype to
another: the ```Z`` quoted symbol tells the operator to try and
convert to an integer. After the conversion, we assign the entire list
to the variable ``masses``.

Now that the data is read in and in the proper form, we need to write
the function to calculate the fuel usage: ::

  fuel:(x:;(x%3)-2);

This function implements the requirement specified in the problem
"Fuel required to launch a given module is based on its
mass. Specifically, to find the fuel required for a module, take its
mass, divide by three, round down, and subtract 2." Note that ``%`` is
used for division instead of the traditional forward slash. Now let's
calculate the fuel required and print it to the screen: ::

  0n:sum fuel masses;

Since each operation in the ``fuel`` definition applies to lists, we
don't need to use the map operator. The ``sum`` function simply adds
up all the numbers in a list and is equivalent to ``+/``. Next we
assign the result to null, represented by ``0n`` in *xs*. This has the
effect of printing the result to the screen.

The Advent of Code site doesn't display the part 2 problem until part
1 has been submitted, so, if you haven't already, create an account
and execute the *xs* code on your given input in order to view
part 2. Come back here once you've read it.

Now that part 1 is out of the way, here's part 2: ::

  0n:sum(sum(where 0<x)@x~fuel fixes)'fl;

What a doozy! Let's piece it apart, identifier by identifier. Part two
calls for recursively calculating the weight of the fuel, so like
before we'll need to apply some function to the fuel masses and then
add them all up. We first apply the ``fixes`` function, which is
similar to ``\`` (scan), except that it continues applying the given
function until one of two conditions is met:

1) Two consecutive results are equal
2) The output equals the initial input

``fuel`` is passed as the input to ``fixes`` and is applied until two
consecutive outputs are the same, which means that we don't need to
calculate for anymore fuel. But because the fuel amount could be
negative, we need to remove the negative numbers from the list. The
``@`` operator takes either a number of a list and returns those
elements of a list. For example: ::

  xs> ([0 2])@[1 3 2]
  0: [1 2]

The ``where`` function returns the indices of the list if given a list
of binary values: ::

  xs> where [1b 0b 1b]
  0: [0 2]

If given a list of integer values, it works like this: ::

  xs> where [1 2 3 1]
  0: [0 1 1 2 2 2 3]

So we use the ``0<x`` expression to return a binary list, indicating
where the positive numbers are; then, we use ``where`` to return the
indices of the list; and finally, we use the ``@`` operator to select
out those indices. Now that we have our list, we simply ``sum`` it in
order to get the total fuel requirement for that module. We ``sum``
to calculate the total fuel requirement for all modules.

Below is the script in full: ::

  masses:(`Z of)'readl "ex/1.txt";        ! read in data
  fuel:(x:;(neg 2)+x%3);                  ! fuel function, broadcasts implicitly
  fl:fuel masses;                         ! calculate masses for each module
  0n:sum fl;                              ! sum masses for part 1
  0n:sum(sum(where 0<x)@x~fuel fixes)'fl; ! recursively calculate and sum

In order to run the script in full, save the above in a file (*xs*
uses the ``.xs`` extension), and run with: ::

  $ xs /path/to/file

Now that you're finished reading this tutorial, you're ready to check
out the API documentation for a full reference of all *xs* functions
and operators! *xs* is a unique language and might take a little bit
getting used to. If you have any questions, don't hesitate to open an
issue on github at the `project page`_ or shoot  me an email at
`sturm@cryptm.org`_

.. _sturm@cryptm.org: mailto://sturm@cryptm.org
.. _project page: https://github.com/smabie/xs
.. _Day 1 Advent of Code 2019: https://adventofcode.com/2019/day/1
