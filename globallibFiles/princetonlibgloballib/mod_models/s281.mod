# This model is dumb.  The cube root can be removed.

param N := 10;

var x{1..N}:=0;

minimize f:
(sum {i in 1..N} i^3*(x[i]-1)^2)^(1/3);

