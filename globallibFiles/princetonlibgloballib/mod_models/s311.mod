param N := 2;

var x{1..N};

minimize f:
(x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2;

data;
var x:=
1	1
2	1;

