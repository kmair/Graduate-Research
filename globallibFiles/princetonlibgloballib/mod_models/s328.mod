param N := 2;

var x{1..N};

minimize f:
0.1*(12+x[1]^2 + (1+x[2]^2)/(x[1]^2) + (x[1]^2*x[2]^2+100)/(x[1]^4*x[2]^4));

subject to cons1{i in 1..2}:
1 <= x[i] <= 3;

data;
var x:=
1	0.5
2	0.5;

