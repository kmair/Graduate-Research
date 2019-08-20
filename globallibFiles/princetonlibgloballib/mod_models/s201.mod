param N := 2;

var x{1..N};

minimize f:
4*(x[1]-5)^2+(x[2]-6)^2;

data;
var x:=
1	8
2	9;

