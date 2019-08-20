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
	4 /* nvar */,
	1 /* nobj */,
	0 /* ncon */,
	0 /* nzc */,
	0 /* densejac */,

	/* objtype (0 = minimize, 1 = maximize) */

	0 };

 real boundc_[1+8+0] /* Infinity, variable bounds, constraint bounds */ = {
		1.7e80,
		0.,
		0.42,
		0.,
		0.42,
		0.,
		0.42,
		0.,
		0.42};

 real x0comn_[4] = {
		0.42,
		0.42,
		0.42,
		0.42 };

 real
feval0_(fint *nobj, real *x)
{
	real v[4];


  /***  objective ***/

	v[0] = 4. * x[1];
	v[1] = 16. + v[0];
	v[0] = x[0] * v[1];
	v[1] = 4. * x[2];
	v[1] += 16.;
	v[1] += x[3];
	if (v[1] == 0.) {
	zerdiv_(&v[1]);	}
	v[2] = v[0] / v[1];
	v[0] = 0.1957 - v[2];
	v[2] = v[0] * v[0];
	v[0] = 2. * x[1];
	v[1] = 4. + v[0];
	v[0] = x[0] * v[1];
	v[1] = 2. * x[2];
	v[1] += 4.;
	v[1] += x[3];
	if (v[1] == 0.) {
	zerdiv_(&v[1]);	}
	v[3] = v[0] / v[1];
	v[0] = 0.1947 - v[3];
	v[3] = v[0] * v[0];
	v[2] += v[3];
	v[3] = 1. + x[1];
	v[0] = x[0] * v[3];
	v[3] = 1. + x[2];
	v[3] += x[3];
	if (v[3] == 0.) {
	zerdiv_(&v[3]);	}
	v[1] = v[0] / v[3];
	v[0] = 0.1735 - v[1];
	v[1] = v[0] * v[0];
	v[2] += v[1];
	v[1] = 0.5 * x[1];
	v[0] = 0.25 + v[1];
	v[1] = x[0] * v[0];
	v[0] = 0.5 * x[2];
	v[0] += 0.25;
	v[0] += x[3];
	if (v[0] == 0.) {
	zerdiv_(&v[0]);	}
	v[3] = v[1] / v[0];
	v[1] = 0.16 - v[3];
	v[3] = v[1] * v[1];
	v[2] += v[3];
	v[3] = 0.25 * x[1];
	v[1] = 0.0625 + v[3];
	v[3] = x[0] * v[1];
	v[1] = 0.25 * x[2];
	v[1] += 0.0625;
	v[1] += x[3];
	if (v[1] == 0.) {
	zerdiv_(&v[1]);	}
	v[0] = v[3] / v[1];
	v[3] = 0.0844 - v[0];
	v[0] = v[3] * v[3];
	v[2] += v[0];
	v[0] = 0.16666666666666666 * x[1];
	v[3] = 0.027777777777777776 + v[0];
	v[0] = x[0] * v[3];
	v[3] = 0.16666666666666666 * x[2];
	v[3] += 0.027777777777777776;
	v[3] += x[3];
	if (v[3] == 0.) {
	zerdiv_(&v[3]);	}
	v[1] = v[0] / v[3];
	v[0] = 0.0627 - v[1];
	v[1] = v[0] * v[0];
	v[2] += v[1];
	v[1] = 0.125 * x[1];
	v[0] = 0.015625 + v[1];
	v[1] = x[0] * v[0];
	v[0] = 0.125 * x[2];
	v[0] += 0.015625;
	v[0] += x[3];
	if (v[0] == 0.) {
	zerdiv_(&v[0]);	}
	v[3] = v[1] / v[0];
	v[1] = 0.0456 - v[3];
	v[3] = v[1] * v[1];
	v[2] += v[3];
	v[3] = 0.1 * x[1];
	v[1] = 0.010000000000000002 + v[3];
	v[3] = x[0] * v[1];
	v[1] = 0.1 * x[2];
	v[1] += 0.010000000000000002;
	v[1] += x[3];
	if (v[1] == 0.) {
	zerdiv_(&v[1]);	}
	v[0] = v[3] / v[1];
	v[3] = 0.0342 - v[0];
	v[0] = v[3] * v[3];
	v[2] += v[0];
	v[0] = 0.08333333333333333 * x[1];
	v[3] = 0.006944444444444444 + v[0];
	v[0] = x[0] * v[3];
	v[3] = 0.08333333333333333 * x[2];
	v[3] += 0.006944444444444444;
	v[3] += x[3];
	if (v[3] == 0.) {
	zerdiv_(&v[3]);	}
	v[1] = v[0] / v[3];
	v[0] = 0.0323 - v[1];
	v[1] = v[0] * v[0];
	v[2] += v[1];
	v[1] = 0.07142857142857142 * x[1];
	v[0] = 0.00510204081632653 + v[1];
	v[1] = x[0] * v[0];
	v[0] = 0.07142857142857142 * x[2];
	v[0] += 0.00510204081632653;
	v[0] += x[3];
	if (v[0] == 0.) {
	zerdiv_(&v[0]);	}
	v[3] = v[1] / v[0];
	v[1] = 0.0235 - v[3];
	v[3] = v[1] * v[1];
	v[2] += v[3];
	v[3] = 0.0625 * x[1];
	v[1] = 0.00390625 + v[3];
	v[3] = x[0] * v[1];
	v[1] = 0.0625 * x[2];
	v[1] += 0.00390625;
	v[1] += x[3];
	if (v[1] == 0.) {
	zerdiv_(&v[1]);	}
	v[0] = v[3] / v[1];
	v[3] = 0.0246 - v[0];
	v[0] = v[3] * v[3];
	v[2] += v[0];

	return v[2];
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

x_input = malloc (4 * sizeof(real));
input_values = malloc (4 * sizeof(double));
c_val = malloc (0 * sizeof(real));

file_input = fopen("input.in","r");
for (i=0; i < 4; i++)
    fscanf(file_input, "%lf" ,&input_values[i]);

fclose(file_input);
for (i=0; i < 4; i++)
 {
    x_input[i] = input_values[i];
 }

f_val = feval0_(&objective_number, x_input);

FILE *output_out;
output_out = fopen ("output.out","w");
fprintf(output_out,"%30.15f\n",f_val);
fclose(output_out);

}
