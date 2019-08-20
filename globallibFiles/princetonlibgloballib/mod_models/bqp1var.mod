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

#   Source: a one variable box-constrained quadratic

#   SIF input: Nick Gould, March 1992

#   classification QBR2-AN-1-0

var x1:=0.25;
minimize f:
	x1+x1^2;
subject to cons1:
	0.0 <= x1 <= 0.5;