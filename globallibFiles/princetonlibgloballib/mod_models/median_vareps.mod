# Objective  function: convex nonlinear
# Constraint functions: none

# Computes the median of m numbers by minimizing
# the sum of the distances.  When m is odd, median
# lies at the middle data point.  The corresponding
# distance term becomes almost "singular" but doesn't
# create any problems.

var eps >= 1.0e-8;
param m;
param a {1..m};

var x;

minimize sum_dists: eps + sum {i in 1..m} sqrt(eps^2+(x-a[i])^2);

data;
param m := 19;
let eps := 1;

let {i in 1..m} a[i] := Uniform01();

let x := 0.5;

