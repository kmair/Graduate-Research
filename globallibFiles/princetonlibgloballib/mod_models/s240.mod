param N := 3;

var x{1..N};

minimize f:
(x[1]-x[2]+x[3])^2 + (-x[1]+x[2]+x[3])^2 + (x[1]+x[2]-x[3])^2;

data;
var x:=
1	100
2	-1
3	2.5;
