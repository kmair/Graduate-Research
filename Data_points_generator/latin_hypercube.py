#! /usr/bin/env python3
#
def latin_random ( dim_num, point_num, seed ):

#*****************************************************************************80
#
## LATIN_RANDOM returns points in a Latin Random square.
#
#  Discussion:
#
#    In each spatial dimension, there will be exactly one
#    point whose coordinate value lies between consecutive
#    values in the list:
#
#      ( 0, 1, 2, ..., point_num ) / point_num
#
#  Licensing:
#
#    This code is distributed under the GNU LGPL license.
#
#  Modified:
#
#    13 November 2014
#
#  Author:
#
#    John Burkardt
#
#  Parameters:
#
#    Input, integer DIM_NUM, the spatial dimension.
#
#    Input, integer POINT_NUM, the number of points.
#
#    Input/output, integer SEED, a seed for the random
#    number generator.
#
#    Output, real X(DIM_NUM,POINT_NUM), the points.
#
  from perm_uniform import perm_uniform
  from r8mat_uniform_01 import r8mat_uniform_01
#
#  Pick DIM_NUM * POINT_NUM random numbers between 0 and 1.
#
  x, seed = r8mat_uniform_01 ( dim_num, point_num, seed )
#
#  For spatial dimension I,
#    pick a random permutation of 1 to POINT_NUM,
#    force the corresponding I-th components of X to lie in the
#    interval ( PERM(J)-1, PERM(J) ) / POINT_NUM.
#
  for i in range ( 0, dim_num ):

    perm, seed = perm_uniform ( point_num, seed )

    for j in range ( 0, point_num ):

      x[i,j] = ( perm[j] + x[i,j] ) / point_num

  return x, seed



def latin_random_hypercube ( seed, I, M ):

#*****************************************************************************80
#
## TEST01 tests LATIN_RANDOM.
#
#  Licensing:
#
#    This code is distributed under the GNU LGPL license.
#
#  Modified:
#
#    13 November 2014
#
#  Author:
#
#    John Burkardt
#
#  Parameters:
#
#    Input/output, integer SEED, a seed for the random number
#    generator.
#
  import platform
  # from r8mat_transpose_print import r8mat_transpose_print

  dim_num = M
  point_num = I

  x, seed = latin_random ( dim_num, point_num, seed )
  datapts = x.T
  
  return datapts

import numpy as np

def main(I, M):
  seed = np.random.randint(100_000)
  datapts = latin_random_hypercube ( seed, I, M )
  return datapts
  # timestamp ( )
