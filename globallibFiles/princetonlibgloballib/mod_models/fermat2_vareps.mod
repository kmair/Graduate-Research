var eps >= 1.0e-8;

param n := 2;		# dimension
param m := 3;		# number of points

param a {1..m, 1..n};

var x {1..n};

minimize sumLone: 
   eps +
   sum {i in 1..m} sqrt( eps^2 + sum {j in 1..n} (x[j] - a[i,j])^2 );

let eps := 1;

let a[1,1] := 0;  let a[1,2] := 0;
let a[2,1] := 4;  let a[2,2] := 0;
let a[3,1] := 2;  let a[3,2] := 1;

let x[1] := 1; let x[2] := 0;
