param N := 3;

var x{1..N};

minimize f:
100*(x[3] - ((x[1]+x[2])/2)^2)^2 + (1-x[1])^2 + (1-x[2])^2;

data;
var x:=
1	-1.2
2	2
3	0;

