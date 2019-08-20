param N := 4;

var x{1..N};
param t{i in 1..20} := 0.2*i;
minimize f:
sum {i in 1..20} ((x[1]+x[2]*t[i]-exp(t[i]))^2 +(x[3]+x[4]*sin(t[i])-cos(t[i]))^2);

data;
var x:=
1	25
2	5
3	-5
4	-1;

