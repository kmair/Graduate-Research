param N := 30;

var x{1..N} := -1.2;

minimize f:
sum {k in 1..29} (100*(x[k+1]-x[k]^2)^2+(1-x[k])^2);


