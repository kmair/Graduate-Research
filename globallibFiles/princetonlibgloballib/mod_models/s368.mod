param N := 8;

var x{1..N} := 1;

minimize f:
-1*(sum {i in 1..8} x[i]^2)*(sum {i in 1..8} x[i]^4) + (sum {i in 1..8} x[i]^3)^2;

subject to cons1{i in 1..8}:
0 <= x[i] <= 1;


