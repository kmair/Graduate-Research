param N := 6;

var x{1..N};

param t{i in 1..13} := i/10;
param y{i in 1..13} := exp(-1*t[i]) - 5*exp(-10*t[i]) + 3*exp(-4*t[i]);
minimize f:
sum {i in 1..13} (x[4]*exp(-1*x[1]*t[i]) - x[5]*exp(-1*x[2]*t[i]) + x[6]*exp(-1*x[3]*t[i]) - y[i])^2;


data;
var x:=
1	1
2	2
3	1
4	1
5	1
6	1;

#edited by Luis M Rios luisrios@uiuc.edu
#imposed bound constraints on variables
s.t. Box1:
     -100 <= x[1] <= 100;
s.t. Box2:
     -100 <= x[2] <= 100;
s.t. Box3:     
     -100 <= x[3] <= 100;     