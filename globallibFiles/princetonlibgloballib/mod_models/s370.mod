param N := 6;

var x{1..N} := 0;

minimize f:
x[1]^2 + (x[2] - x[1]^2 - 1)^2 + sum {i in 2..30} (sum {j in 2..6} (j-1)*x[j]*((i-1)/29)^(j-2) - (sum {j in 1..6} 
x[j]*((i-1)/29)^(j-1))^2 - 1)^2;

