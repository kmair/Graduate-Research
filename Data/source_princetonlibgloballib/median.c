#include "math.h"
#include "errno.h"
#ifndef fint
#ifndef Long
#include "arith.h"	/* for Long */
#ifndef Long
#define Long long
#endif
#endif
#define fint Long
#endif
#ifndef real
#define real double
#endif
#ifdef __cplusplus
extern "C" {
#endif
 real acosh_(real *);
 real asinh_(real *);
 real acoshd_(real *, real *);
 real asinhd_(real *, real *);
 void in_trouble(char *, real);
 void in_trouble2(char *, real, real);
 void domain_(char *, real *, fint);
 void zerdiv_(real *);
 fint auxcom_[1] = { 0 /* nlc */ };
 fint funcom_[6] = {
	1 /* nvar */,
	1 /* nobj */,
	0 /* ncon */,
	0 /* nzc */,
	0 /* densejac */,

	/* objtype (0 = minimize, 1 = maximize) */

	0 };

 real boundc_[1+2+0] /* Infinity, variable bounds, constraint bounds */ = {
		1.7e80,
		-1.7e80,
		1.7e80};

 real x0comn_[1] = {
		0.5 };

 real
feval0_(fint *nobj, real *x)
{
	real v[3];


  /***  objective ***/

	v[0] = -0.171747132 + x[0];
	if (v[0] < 0.) {
	v[1] = -v[0];
	} else {
	v[1] = v[0];
	}
	v[0] = -0.843266708 + x[0];
	if (v[0] < 0.) {
	v[2] = -v[0];
	} else {
	v[2] = v[0];
	}
	v[1] += v[2];
	v[2] = -0.550375356 + x[0];
	if (v[2] < 0.) {
	v[0] = -v[2];
	} else {
	v[0] = v[2];
	}
	v[1] += v[0];
	v[0] = -0.301137904 + x[0];
	if (v[0] < 0.) {
	v[2] = -v[0];
	} else {
	v[2] = v[0];
	}
	v[1] += v[2];
	v[2] = -0.292212117 + x[0];
	if (v[2] < 0.) {
	v[0] = -v[2];
	} else {
	v[0] = v[2];
	}
	v[1] += v[0];
	v[0] = -0.2240528679 + x[0];
	if (v[0] < 0.) {
	v[2] = -v[0];
	} else {
	v[2] = v[0];
	}
	v[1] += v[2];
	v[2] = -0.349830504 + x[0];
	if (v[2] < 0.) {
	v[0] = -v[2];
	} else {
	v[0] = v[2];
	}
	v[1] += v[0];
	v[0] = -0.856270347 + x[0];
	if (v[0] < 0.) {
	v[2] = -v[0];
	} else {
	v[2] = v[0];
	}
	v[1] += v[2];
	v[2] = -0.067113723 + x[0];
	if (v[2] < 0.) {
	v[0] = -v[2];
	} else {
	v[0] = v[2];
	}
	v[1] += v[0];
	v[0] = -0.500210669 + x[0];
	if (v[0] < 0.) {
	v[2] = -v[0];
	} else {
	v[2] = v[0];
	}
	v[1] += v[2];
	v[2] = -0.998117627 + x[0];
	if (v[2] < 0.) {
	v[0] = -v[2];
	} else {
	v[0] = v[2];
	}
	v[1] += v[0];
	v[0] = -0.578733378 + x[0];
	if (v[0] < 0.) {
	v[2] = -v[0];
	} else {
	v[2] = v[0];
	}
	v[1] += v[2];
	v[2] = -0.991133039 + x[0];
	if (v[2] < 0.) {
	v[0] = -v[2];
	} else {
	v[0] = v[2];
	}
	v[1] += v[0];
	v[0] = -0.762250467 + x[0];
	if (v[0] < 0.) {
	v[2] = -v[0];
	} else {
	v[2] = v[0];
	}
	v[1] += v[2];
	v[2] = -0.130692483 + x[0];
	if (v[2] < 0.) {
	v[0] = -v[2];
	} else {
	v[0] = v[2];
	}
	v[1] += v[0];
	v[0] = -0.639718759 + x[0];
	if (v[0] < 0.) {
	v[2] = -v[0];
	} else {
	v[2] = v[0];
	}
	v[1] += v[2];
	v[2] = -0.159517864 + x[0];
	if (v[2] < 0.) {
	v[0] = -v[2];
	} else {
	v[0] = v[2];
	}
	v[1] += v[0];
	v[0] = -0.250080533 + x[0];
	if (v[0] < 0.) {
	v[2] = -v[0];
	} else {
	v[2] = v[0];
	}
	v[1] += v[2];
	v[2] = -0.668928609 + x[0];
	if (v[2] < 0.) {
	v[0] = -v[2];
	} else {
	v[0] = v[2];
	}
	v[1] += v[0];

	return v[1];
}

 void
ceval0_(real *x, real *c)
{}
#ifdef __cplusplus
	}
#endif

#include <stdio.h>
#include <stdlib.h>

main(int argc, char **argv)
{

FILE *file_input;
real *x_input, f_val, *c_val;
double *input_values;
fint objective_number;
int i;

x_input = malloc (1 * sizeof(real));
input_values = malloc (1 * sizeof(double));
c_val = malloc (0 * sizeof(real));

file_input = fopen("input.in","r");
for (i=0; i < 1; i++)
    fscanf(file_input, "%lf" ,&input_values[i]);

fclose(file_input);
for (i=0; i < 1; i++)
 {
    x_input[i] = input_values[i];
 }

f_val = feval0_(&objective_number, x_input);

FILE *output_out;
output_out = fopen ("output.out","w");
fprintf(output_out,"%30.15f\n",f_val);
fclose(output_out);

}
