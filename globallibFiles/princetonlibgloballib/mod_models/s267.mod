param N := 5;

var x{1..N};
param z{i in 1..11} := 0.1*i;
param y{i in 1..11} := exp(-1*z[i]) - 5*exp(-10*z[i]) + 3*exp(-4*z[i]);

minimize f:
sum {i in 1..11} (x[3]*exp(-1*x[1]*z[i]) - x[4]*exp(-1*x[2]*z[i]) + 3*exp(-1*x[5]*z[i]) - y[i])^2;

data;
var x:=
1	2
2	2
3	2
4	2
5	2;

#edited by Luis M Rios luisrios@uiuc.edu
#imposed bound constraints on variables
s.t. Box1:
     -100 <= x[1] <= 100;
s.t. Box2:
     -100 <= x[2] <= 100;
s.t. Box3:     
     -100 <= x[5] <= 100;     