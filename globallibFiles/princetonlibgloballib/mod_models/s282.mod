param N := 10;

var x{1..N};

minimize f:
(x[1]-1)^2+(x[10]-1)^2+10*(sum{i in 1..9} (10-i)*(x[i]^2-x[i+1])^2);

data;
var x:=
1	-1.2
2	0
3	0
4	0
5	0
6	0
7	0
8	0
9	0
10	0;

