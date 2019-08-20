param N := 3;

var x{1..N};

minimize f:
sum {i in 1..10} ((exp(-1*i*x[1]/10) - exp(-1*i*x[2]/10)) - x[3]*(exp(-1*i/10) - exp(-1*i)))^2;

data;
var x:=
1	0
2	10
3	20;

#edited by Luis M Rios luisrios@uiuc.edu
#imposed bound constraints on variables
s.t. Box1:
     -50 <= x[1] <= 50;
s.t. Box2:
     -50 <= x[2] <= 50;    