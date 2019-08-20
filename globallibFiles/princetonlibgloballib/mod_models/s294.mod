param N := 6;

var x{1..N};

minimize f:
sum {k in 1..5} (100*(x[k+1]-x[k]^2)^2+(1-x[k])^2);

data;
var x:=
1	-1.2
2	1
3	-1.2
4	1
5	-1.2
6	1;
