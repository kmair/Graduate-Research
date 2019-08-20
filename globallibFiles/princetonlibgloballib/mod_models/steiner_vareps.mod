var eps >= 1.0e-8;

param m;	# number of nodes (original plus steiner)
param m1;	# number of steiner nodes

param a{m1+1..m, 1..2};	# coords of original nodes

set NODES := {1..m};

set ARCS within NODES cross NODES;

var x {NODES, 1..2};

minimize dist: 
    eps+
    sum {(i,j) in ARCS} sqrt( eps^2 + sum {k in 1..2} (x[i,k]-x[j,k])^2);

data;

param m := 18;
param m1 := 8;
param a: 1  2 :=
  9  2.309469 9.208211
 10  0.577367 6.480938
 11  0.808314 3.519062
 12  1.685912 1.231672
 13  4.110855 0.821114
 14  7.598152 0.615836
 15  8.568129 3.079179
 16  4.757506 3.753666
 17  3.926097 7.008798
 18  7.436490 7.683284
 ;

set ARCS := 
  9  7
 10  1
 11  2
 12  3
 13  4
 14  5
 15  5
 16  6
 17  8
 18  8
  5  6
  6  4
  4  3
  3  2
  2  1
  1  7
  7  8
 ;

let eps := 1;

fix {j in m1+1..m, k in 1..2} x[j,k] := a[j,k];
let {j in 1..m1, k in 1..2} x[j,k] := Uniform01();

