param N := 2;

var x{1..N};

minimize f:
(4*(x[1]+x[2]))^2 + (4*(x[1]+x[2]) + (x[1]-x[2])*((x[1]-2)^2+x[2]^2 - 1))^2;

data;
var x:=
1	2
2	0;

