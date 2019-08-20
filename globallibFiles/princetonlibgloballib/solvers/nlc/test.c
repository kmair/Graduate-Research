#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "errno.h"

main (int argc, char **argv)
{
  double numbval = 0;
  printf("Hola\n");
  printf ("7 ^ 3 = %lf\n", pow(7,3));
  printf ("4.73 ^ 12 = %lf\n", pow(4.73,12));
  printf ("32.01 ^ 1.54 = %lf\n", pow(32.01,1.54));
  printf (" 0 ^ 2 = %lf\n", pow(numbval,2.0));
  if (errno) printf("Introuble\n");
  printf ("Hola");
}
