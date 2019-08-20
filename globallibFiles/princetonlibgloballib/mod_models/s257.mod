param N := 4;

var x{1..N};

minimize f:
100*(x[1]^2-x[2])^2 + (x[1]-1)^2 + 90*(x[3]^2-x[4])^2 + (x[3]-1)^2 + 10.1*((x[2] - 1)^2 + (x[4] - 1)^2) + 
19.8*(x[1]-1)*(x[ 4]-1);

subject to cons1:
x[1] >= 0;

subject to cons3:
x[3] >= 0;

data;
var x:=
1	-3
2	-1
3	-3
4	-1;

