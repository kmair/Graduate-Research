param N := 20;

var x{i in 1..N} := if (i <= 10) then -1.2 else 1;

minimize f:
sum{i in 1..10} (100*(x[i]^2 - x[i+10])^2+(x[i]-1)^2);

