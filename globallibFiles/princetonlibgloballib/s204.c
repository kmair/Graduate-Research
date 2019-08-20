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
	2 /* nvar */,
	1 /* nobj */,
	0 /* ncon */,
	0 /* nzc */,
	0 /* densejac */,

	/* objtype (0 = minimize, 1 = maximize) */

	0 };

 real boundc_[1+4+0] /* Infinity, variable bounds, constraint bounds */ = {
		1.7e80,
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80};

 real x0comn_[2] = {
		0.1,
		0.1 };

 real
feval0_(fint *nobj, real *x)
{
	real v[11];


	/*** defined variable 1 ***/

	v[0] = 5.66598 * x[0];
	v[1] = 2.77141 * x[1];
	v[2] = v[0] + v[1];
	v[0] = x[0] * v[2];
	v[2] = 1.2537 * v[0];
	v[0] = 2.77141 * x[0];
	v[1] = 2.12413 * x[1];
	v[3] = v[0] + v[1];
	v[0] = x[1] * v[3];
	v[3] = 1.2537 * v[0];
	v[8] = v[2] + v[3];
	v[8] += 0.13294;
	v[8] = v[8] - 0.564255*x[0];
	v[8] += 0.392417*x[1];

	/*** defined variable 2 ***/

	v[3] = 5.66598 * x[0];
	v[0] = 2.77141 * x[1];
	v[1] = v[3] + v[0];
	v[3] = x[0] * v[1];
	v[1] = -0.682005 * v[3];
	v[3] = 2.77141 * x[0];
	v[0] = 2.12413 * x[1];
	v[4] = v[3] + v[0];
	v[3] = x[1] * v[4];
	v[4] = -0.682005 * v[3];
	v[9] = v[1] + v[4];
	v[9] += -0.244378;
	v[9] = v[9] - 0.404979*x[0];
	v[9] += 0.927589*x[1];

	/*** defined variable 3 ***/

	v[4] = 5.66598 * x[0];
	v[3] = 2.77141 * x[1];
	v[0] = v[4] + v[3];
	v[4] = x[0] * v[0];
	v[0] = 0.51141 * v[4];
	v[4] = 2.77141 * x[0];
	v[3] = 2.12413 * x[1];
	v[5] = v[4] + v[3];
	v[4] = x[1] * v[5];
	v[5] = 0.51141 * v[4];
	v[10] = v[0] + v[5];
	v[10] += 0.325895;
	v[10] = v[10] - 0.0735084*x[0];
	v[10] += 0.535493*x[1];

  /***  objective ***/

	v[6] = v[8] * v[8];
	v[7] = v[9] * v[9];
	v[6] += v[7];
	v[7] = v[10] * v[10];
	v[6] += v[7];

	return v[6];
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

x_input = malloc (2 * sizeof(real));
input_values = malloc (2 * sizeof(double));
c_val = malloc (0 * sizeof(real));

file_input = fopen("input.in","r");
for (i=0; i < 2; i++)
    fscanf(file_input, "%lf" ,&input_values[i]);

fclose(file_input);
for (i=0; i < 2; i++)
 {
    x_input[i] = input_values[i];
 }

f_val = feval0_(&objective_number, x_input);

FILE *output_out;
output_out = fopen ("output.out","w");
fprintf(output_out,"%30.15f\n",f_val);
fclose(output_out);

}
