param N := 4;

var x{1..N};

minimize f:
(exp(x[1]) - x[2])^4 + 100*(x[2]-x[3])^6+(tan(x[3]-x[4]))^4+x[1]^8+(x[4]-1)^2;

data;
var x:=
1	0
2	0
3	0
4	0;

#edited by Luis M Rios luisrios@uiuc.edu
#imposed bound constraints on variables
s.t. Box1:
     -40 <= x[1] <= 40;    