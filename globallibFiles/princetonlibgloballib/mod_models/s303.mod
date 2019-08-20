param N := 20;

var x{1..N} := 0.1;

minimize f:
sum {i in 1..N} x[i]^2 + (sum {i in 1..N} i*x[i]/2)^2 + (sum {i in 1..N} i*x[i]/2)^4;


