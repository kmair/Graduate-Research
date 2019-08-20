param N := 2;

var x{1..N};

minimize f:
(10*(x[1] - x[2])^2 + (x[1]-1)^2)^4;

data;
var x:=
1	3
2	1;

