param N := 10;

var x{1..N} := 1;
param A{i in 1..N, j in 1..N} := if (i=j) then i else 0;

minimize f:
sum {i in 1..N} x[i]*(sum {j in 1..N} A[i,j]*x[j]);


