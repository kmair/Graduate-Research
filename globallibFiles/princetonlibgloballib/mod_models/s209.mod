param N := 2;

var x{1..N};

minimize f:
10^4*(x[2] - x[1]^2)^2 + (1-x[1])^2;

data;
var x:=
1	-1.2
2	1;

