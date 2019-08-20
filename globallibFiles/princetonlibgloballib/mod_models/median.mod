# Objective  function: convex nonlinear
# Constraint functions: none

# Computes the median of m numbers by minimizing
# the sum of the distances.  When m is odd, median
# lies at the middle data point.  The corresponding
# distance term becomes "singular"
# and loqo fails.

param m;
param a {1..m};

var x;

minimize sum_dists: sum {i in 1..m} abs(x-a[i]);

data;
param m := 19;
let {i in 1..m} a[i] := Uniform01();

let x := 0.5;

#values of a are taken from the gams version of median
let a[1] :=     0.171747132;
let a[2] :=     0.843266708;
let a[3] :=     0.550375356;
let a[4] :=     0.301137904;
let a[5] :=     0.292212117;
let a[6] :=     0.2240528679;
let a[7] :=     0.349830504;
let a[8] :=     0.856270347;
let a[9] :=     0.067113723;
let a[10] :=    0.500210669;
let a[11] :=    0.998117627;
let a[12] :=    0.578733378;
let a[13] :=    0.991133039;
let a[14] :=    0.762250467;
let a[15] :=    0.130692483;
let a[16] :=    0.639718759;
let a[17] :=    0.159517864;
let a[18] :=    0.250080533;
let a[19] :=    0.668928609;
