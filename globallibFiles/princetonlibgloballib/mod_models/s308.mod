param N := 2;

var x{1..N};

minimize f:
(x[1]^2+x[2]^2+x[1]*x[2])^2 + sin(x[1])^2 + cos(x[2])^2;

data;
var x:=
1	3
2	0.1;

