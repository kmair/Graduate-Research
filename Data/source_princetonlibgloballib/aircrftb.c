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
	5 /* nvar */,
	1 /* nobj */,
	0 /* ncon */,
	0 /* nzc */,
	0 /* densejac */,

	/* objtype (0 = minimize, 1 = maximize) */

	0 };

 real boundc_[1+10+0] /* Infinity, variable bounds, constraint bounds */ = {
		1.7e80,
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80};

 real x0comn_[5] = {
		0.,
		0.,
		0.,
		0.,
		0. };

 real
feval0_(fint *nobj, real *x)
{
	real v[18];


	/*** defined variable 1 ***/

	v[8] = -4.583 - 3.933*x[0];
	v[8] += 0.107*x[1];
	v[8] += 0.126*x[2];
	v[8] -= 9.99*x[4];

	/*** defined variable 2 ***/

	v[9] = 1.4185 - 0.987*x[1];
	v[9] -= 22.95*x[3];

	/*** defined variable 3 ***/

	v[10] = -0.09210000000000002 + 0.002*x[0];
	v[10] -= 0.235*x[2];
	v[10] += 5.67*x[4];

	/*** defined variable 4 ***/

	v[11] = 0.008400000000000001 + x[1];
	v[11] -= x[3];

	/*** defined variable 5 ***/

	v[12] = -0.0007100000000000001 - x[2];
	v[12] -= 0.196*x[4];

	/*** defined variable 6 ***/

	v[0] = -0.727 * x[1];
	v[1] = v[0] * x[2];
	v[0] = 8.39 * x[2];
	v[2] = v[0] * x[3];
	v[13] = v[1] + v[2];
	v[2] = 684.4 * x[3];
	v[0] = v[2] * x[4];
	v[2] = -v[0];
	v[13] += v[2];
	v[2] = 63.5 * x[3];
	v[0] = v[2] * x[1];
	v[13] += v[0];

	/*** defined variable 7 ***/

	v[0] = 0.949 * x[0];
	v[2] = v[0] * x[2];
	v[0] = 0.173 * x[0];
	v[3] = v[0] * x[4];
	v[14] = v[2] + v[3];

	/*** defined variable 8 ***/

	v[2] = -0.716 * x[0];
	v[3] = v[2] * x[1];
	v[2] = 1.578 * x[0];
	v[4] = v[2] * x[3];
	v[2] = -v[4];
	v[15] = v[3] + v[2];
	v[2] = 1.132 * x[3];
	v[4] = v[2] * x[1];
	v[15] += v[4];

	/*** defined variable 9 ***/

	v[4] = x[0] * x[4];
	v[16] = -v[4];

	/*** defined variable 10 ***/

	v[17] = x[0] * x[3];

  /***  objective ***/

	v[5] = v[8] + v[13];
	v[6] = v[5] * v[5];
	v[5] = v[9] + v[14];
	v[7] = v[5] * v[5];
	v[6] += v[7];
	v[7] = v[10] + v[15];
	v[5] = v[7] * v[7];
	v[6] += v[5];
	v[5] = v[11] + v[16];
	v[7] = v[5] * v[5];
	v[6] += v[7];
	v[7] = v[12] + v[17];
	v[5] = v[7] * v[7];
	v[6] += v[5];

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

x_input = malloc (5 * sizeof(real));
input_values = malloc (5 * sizeof(double));
c_val = malloc (0 * sizeof(real));

file_input = fopen("input.in","r");
for (i=0; i < 5; i++)
    fscanf(file_input, "%lf" ,&input_values[i]);

fclose(file_input);
for (i=0; i < 5; i++)
 {
    x_input[i] = input_values[i];
 }

f_val = feval0_(&objective_number, x_input);

FILE *output_out;
output_out = fopen ("output.out","w");
fprintf(output_out,"%30.15f\n",f_val);
fclose(output_out);

}
