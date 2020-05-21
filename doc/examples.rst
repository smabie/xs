Examples
========

Factorial: ::

  fac:(prod 1+til);

Fibonnaci: ::

  fib:(n:(neg 1)+;0@n do(2 enlist 1@x sum x~)[0 1]);

Euclid's GCD: ::

  gcd:{([`x`y]):..;if y==0(x)(y gcd x mod y)};

Advent of Code 2019 Day 1: ::

  masses:(`Z of)'readl "ex/1.txt";        ! read in data
  fuel:(x:;(neg 2)+x%3);                  ! fuel function, broadcasts implicitly
  fl:fuel masses;                         ! calculate masses for each module
  0n:sum fl;                              ! sum masses for part 1
  0n:sum(sum(where 0<x)@x~fuel fixes)'fl; ! recursively calculate and sum

Advent of Code 2019 Day 3: ::

  wires:(","vs)'readl "ex/3.txt";
  section:(
    dir:0@x~; len:`Z of 1_x;
    basis:cond[(dir=="R")([1 0])(dir=="L")([neg 1 0])(dir=="U")([0 1])([0 neg 1])];
    2 cut(2*len)#basis
  );
  expand:(sums cat section');
  mdist:(min sum abs flip inter/expand');
  ldist:(2+min sum ((inter/x)?)'x~expand');

  0n:mdist wires;
  0n:ldist wires;
