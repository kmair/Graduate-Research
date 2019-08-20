param N := 2;

var x{1..N};
var g = -1*x[1]^2/4 - x[2]^2 +1;
var h = x[1]-2*x[2]+1;

minimize f:
(x[1]-2)^2+(x[2]-1)^2+0.04/g+h^2/0.2;

data;
var x:=
1	2
2	2;

