param N := 3;

param A {1..3};
param D {1..3};
param H{1..3,1..2};
param B{1..2,1..2};

var x{1..N} := 0.1;
var F{i in 1..3} = A[i] + sum {j in 1..2} H[i,j]*x[j] + 0.5*(sum {j in 1..2} (x[j]*sum {k in 1..2} B[j,k]*x[k]))*D[i];

minimize f:
sum {i in 1..3} F[i]^2;

data;
param A:=
1	0.13294
2	-0.244378
3	0.325895;

param D:=
1	2.5074
2	-1.36401
3	1.02282;

param H:
	1		2:=
1	-0.564255	0.392417 
2	-0.404979	0.927589
3	-0.0735084	0.535493;
param B:
	1			2:=
1	5.66598		2.77141
2	2.77141		2.12413;
