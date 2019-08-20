param N := 3;

var x{1..N};  # loqo works after adding:  >= -200;  
param y{1..8};
param a{1..8};

minimize f:
sum {i in 1..8} ((y[i]-x[1]*exp(-x[2]*a[i]) - x[3])/y[i])^2;

data;
var x:=
1	30
2	0.04
3	3;

param:
	y		a:=
1	72.1		4
2	65.6		5.75
3	55.9		7.5
4	17.1		24
5	9.8		32
6	4.5		48
7	1.3		72
8	0.6		96;

#edited by Luis M Rios luisrios@uiuc.edu
#imposed bound constraints on variables
s.t. Box1:
     -1.5 <= x[2] <= 1.5;