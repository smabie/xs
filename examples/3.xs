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
