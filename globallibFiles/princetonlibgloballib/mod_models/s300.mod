param N := 20;

var x{1..N} := 0;
param A{i in 1..N, j in 1..N} := if (i=j and i=1) then 1 else if (i = j-1 or i = j+1) then -1 else if (i=j and i>1) then 2 else 0;

minimize f:
sum {i in 1..N} x[i]*(sum {j in 1..N} A[i,j]*x[j]) - 2*x[1];


