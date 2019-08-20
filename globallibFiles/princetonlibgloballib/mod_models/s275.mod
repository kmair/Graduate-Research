param N := 4;

var x{1..N};
param A{i in 1..N, j in 1..N} := 1/(i+j-1);

minimize f:
sum {i in 1..N} x[i]*(sum {j in 1..N}A[i,j]*x[j]);

data;
var x:=
1	-4
2	-2
3	-1.333
4	-1;
