param N := 6;

var x{1..N};

minimize f:
10*sum {i in 1..N} (16-i)*(x[i]-1)^2 + 10*(sum {i in 1..N} (16-i)*(x[i]-1)^2)^2;

data;
var x:=
1	0
2	0
3	0
4	0
5	0
6	0;

