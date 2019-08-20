# AMPL Model by Hande Y. Benson
#
# Copyright (C) 2001 Princeton University
# All Rights Reserved
#
# Permission to use, copy, modify, and distribute this software and
# its documentation for any purpose and without fee is hereby
# granted, provided that the above copyright notice appear in all
# copies and that the copyright notice and this
# permission notice appear in all supporting documentation.                     

#   Source: an example problem (p. 98) in
#   J.E. Dennis and R.B. Schnabel,
#   "Numerical Methods for Unconstrained Optimization and Nonlinear
#   Equations",
#   Prentice-Hall, Englewood Cliffs, 1983.

#   SIF input: Ph. Toint, Nov 1990.

#   classification SUR2-AN-2-0

param xinit{1..2};
var x{i in 1..2} := xinit[i];

minimize f:
	(-2+x[1]^2+x[2]^2)^2 + (-2+exp(x[1]-1)+x[2]^3)^2;

data;
param xinit:= 1 2 2 3;

#edited by Luis M Rios luisrios@uiuc.edu
#imposed bound constraints on variables
s.t. Box1:
     -150 <= x[1] <= 150;