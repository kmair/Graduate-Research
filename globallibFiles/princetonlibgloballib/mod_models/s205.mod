param N := 2;

var x{1..N};

minimize f:
(1.5 - x[1]*(1-x[2]))^2 + (2.25 - x[1]*(1 - x[2]^2))^2+ (2.625 - x[1]*(1-x[2]^3))^2;

data;
var x:=
1	0
2	0;
