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
	8 /* nvar */,
	1 /* nobj */,
	0 /* ncon */,
	0 /* nzc */,
	0 /* densejac */,

	/* objtype (0 = minimize, 1 = maximize) */

	0 };

 real boundc_[1+16+0] /* Infinity, variable bounds, constraint bounds */ = {
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
		1.7e80,
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80};

 real x0comn_[8] = {
		0.,
		1.,
		0.,
		1.,
		1.,
		1.,
		1.,
		1. };

 real
feval0_(fint *nobj, real *x)
{
	real v[7];


  /***  objective ***/

	v[0] = 0.69 + x[0];
	v[0] += x[1];
	v[1] = 0.69 + x[0];
	v[1] += x[1];
	v[2] = v[0] * v[1];
	v[0] = 0.044 + x[2];
	v[0] += x[3];
	v[1] = 0.044 + x[2];
	v[1] += x[3];
	v[3] = v[0] * v[1];
	v[2] += v[3];
	v[3] = x[4] * x[0];
	v[0] = x[5] * x[1];
	v[1] = v[3] + v[0];
	v[3] = x[6] * x[2];
	v[0] = v[1] - v[3];
	v[1] = x[7] * x[3];
	v[3] = v[0] - v[1];
	v[0] = 1.57 + v[3];
	v[3] = x[4] * x[0];
	v[1] = x[5] * x[1];
	v[4] = v[3] + v[1];
	v[3] = x[6] * x[2];
	v[1] = v[4] - v[3];
	v[4] = x[7] * x[3];
	v[3] = v[1] - v[4];
	v[1] = 1.57 + v[3];
	v[3] = v[0] * v[1];
	v[2] += v[3];
	v[3] = x[6] * x[0];
	v[3] += 1.31;
	v[0] = x[7] * x[1];
	v[3] += v[0];
	v[0] = x[4] * x[2];
	v[3] += v[0];
	v[0] = x[5] * x[3];
	v[3] += v[0];
	v[0] = x[6] * x[0];
	v[0] += 1.31;
	v[1] = x[7] * x[1];
	v[0] += v[1];
	v[1] = x[4] * x[2];
	v[0] += v[1];
	v[1] = x[5] * x[3];
	v[0] += v[1];
	v[1] = v[3] * v[0];
	v[2] += v[1];
	v[1] = x[4] * x[4];
	v[3] = x[6] * x[6];
	v[0] = v[1] - v[3];
	v[1] = x[0] * v[0];
	v[0] = 2. * x[2];
	v[3] = v[0] * x[4];
	v[0] = v[3] * x[6];
	v[3] = v[1] - v[0];
	v[1] = x[5] * x[5];
	v[0] = x[7] * x[7];
	v[4] = v[1] - v[0];
	v[1] = x[1] * v[4];
	v[4] = v[3] + v[1];
	v[3] = 2. * x[3];
	v[1] = v[3] * x[5];
	v[3] = v[1] * x[7];
	v[1] = v[4] - v[3];
	v[4] = 2.65 + v[1];
	v[1] = x[4] * x[4];
	v[3] = x[6] * x[6];
	v[0] = v[1] - v[3];
	v[1] = x[0] * v[0];
	v[0] = 2. * x[2];
	v[3] = v[0] * x[4];
	v[0] = v[3] * x[6];
	v[3] = v[1] - v[0];
	v[1] = x[5] * x[5];
	v[0] = x[7] * x[7];
	v[5] = v[1] - v[0];
	v[1] = x[1] * v[5];
	v[5] = v[3] + v[1];
	v[3] = 2. * x[3];
	v[1] = v[3] * x[5];
	v[3] = v[1] * x[7];
	v[1] = v[5] - v[3];
	v[5] = 2.65 + v[1];
	v[1] = v[4] * v[5];
	v[2] += v[1];
	v[1] = x[4] * x[4];
	v[4] = x[6] * x[6];
	v[5] = v[1] - v[4];
	v[1] = x[2] * v[5];
	v[5] = 2. * x[0];
	v[4] = v[5] * x[4];
	v[5] = v[4] * x[6];
	v[1] += v[5];
	v[5] = x[5] * x[5];
	v[4] = x[7] * x[7];
	v[3] = v[5] - v[4];
	v[5] = x[3] * v[3];
	v[1] += v[5];
	v[5] = 2. * x[1];
	v[3] = v[5] * x[5];
	v[5] = v[3] * x[7];
	v[1] += v[5];
	v[5] = -2. + v[1];
	v[1] = x[4] * x[4];
	v[3] = x[6] * x[6];
	v[4] = v[1] - v[3];
	v[1] = x[2] * v[4];
	v[4] = 2. * x[0];
	v[3] = v[4] * x[4];
	v[4] = v[3] * x[6];
	v[1] += v[4];
	v[4] = x[5] * x[5];
	v[3] = x[7] * x[7];
	v[0] = v[4] - v[3];
	v[4] = x[3] * v[0];
	v[1] += v[4];
	v[4] = 2. * x[1];
	v[0] = v[4] * x[5];
	v[4] = v[0] * x[7];
	v[1] += v[4];
	v[4] = -2. + v[1];
	v[1] = v[5] * v[4];
	v[2] += v[1];
	v[1] = x[0] * x[4];
	v[5] = x[4] * x[4];
	v[4] = x[6] * x[6];
	v[0] = -3. * v[4];
	v[4] = v[5] + v[0];
	v[5] = v[1] * v[4];
	v[5] += 12.6;
	v[1] = x[2] * x[6];
	v[4] = x[6] * x[6];
	v[0] = x[4] * x[4];
	v[3] = -3. * v[0];
	v[0] = v[4] + v[3];
	v[4] = v[1] * v[0];
	v[5] += v[4];
	v[4] = x[1] * x[5];
	v[1] = x[5] * x[5];
	v[0] = x[7] * x[7];
	v[3] = -3. * v[0];
	v[0] = v[1] + v[3];
	v[1] = v[4] * v[0];
	v[5] += v[1];
	v[1] = x[3] * x[7];
	v[4] = x[7] * x[7];
	v[0] = x[5] * x[5];
	v[3] = -3. * v[0];
	v[0] = v[4] + v[3];
	v[4] = v[1] * v[0];
	v[5] += v[4];
	v[4] = x[0] * x[4];
	v[1] = x[4] * x[4];
	v[0] = x[6] * x[6];
	v[3] = -3. * v[0];
	v[0] = v[1] + v[3];
	v[1] = v[4] * v[0];
	v[1] += 12.6;
	v[4] = x[2] * x[6];
	v[0] = x[6] * x[6];
	v[3] = x[4] * x[4];
	v[6] = -3. * v[3];
	v[3] = v[0] + v[6];
	v[0] = v[4] * v[3];
	v[1] += v[0];
	v[0] = x[1] * x[5];
	v[4] = x[5] * x[5];
	v[3] = x[7] * x[7];
	v[6] = -3. * v[3];
	v[3] = v[4] + v[6];
	v[4] = v[0] * v[3];
	v[1] += v[4];
	v[4] = x[3] * x[7];
	v[0] = x[7] * x[7];
	v[3] = x[5] * x[5];
	v[6] = -3. * v[3];
	v[3] = v[0] + v[6];
	v[0] = v[4] * v[3];
	v[1] += v[0];
	v[0] = v[5] * v[1];
	v[2] += v[0];
	v[0] = x[2] * x[4];
	v[5] = x[4] * x[4];
	v[1] = x[6] * x[6];
	v[4] = -3. * v[1];
	v[1] = v[5] + v[4];
	v[5] = v[0] * v[1];
	v[0] = x[0] * x[6];
	v[1] = x[6] * x[6];
	v[4] = x[4] * x[4];
	v[3] = -3. * v[4];
	v[4] = v[1] + v[3];
	v[1] = v[0] * v[4];
	v[0] = v[5] - v[1];
	v[5] = x[3] * x[5];
	v[1] = x[5] * x[5];
	v[4] = x[7] * x[7];
	v[3] = -3. * v[4];
	v[4] = v[1] + v[3];
	v[1] = v[5] * v[4];
	v[5] = v[0] + v[1];
	v[0] = x[1] * x[7];
	v[1] = x[7] * x[7];
	v[4] = x[5] * x[5];
	v[3] = -3. * v[4];
	v[4] = v[1] + v[3];
	v[1] = v[0] * v[4];
	v[0] = v[5] - v[1];
	v[5] = -9.48 + v[0];
	v[0] = x[2] * x[4];
	v[1] = x[4] * x[4];
	v[4] = x[6] * x[6];
	v[3] = -3. * v[4];
	v[4] = v[1] + v[3];
	v[1] = v[0] * v[4];
	v[0] = x[0] * x[6];
	v[4] = x[6] * x[6];
	v[3] = x[4] * x[4];
	v[6] = -3. * v[3];
	v[3] = v[4] + v[6];
	v[4] = v[0] * v[3];
	v[0] = v[1] - v[4];
	v[1] = x[3] * x[5];
	v[4] = x[5] * x[5];
	v[3] = x[7] * x[7];
	v[6] = -3. * v[3];
	v[3] = v[4] + v[6];
	v[4] = v[1] * v[3];
	v[1] = v[0] + v[4];
	v[0] = x[1] * x[7];
	v[4] = x[7] * x[7];
	v[3] = x[5] * x[5];
	v[6] = -3. * v[3];
	v[3] = v[4] + v[6];
	v[4] = v[0] * v[3];
	v[0] = v[1] - v[4];
	v[1] = -9.48 + v[0];
	v[0] = v[5] * v[1];
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

x_input = malloc (8 * sizeof(real));
input_values = malloc (8 * sizeof(double));
c_val = malloc (0 * sizeof(real));

file_input = fopen("input.in","r");
for (i=0; i < 8; i++)
    fscanf(file_input, "%lf" ,&input_values[i]);

fclose(file_input);
for (i=0; i < 8; i++)
 {
    x_input[i] = input_values[i];
 }

f_val = feval0_(&objective_number, x_input);

FILE *output_out;
output_out = fopen ("output.out","w");
fprintf(output_out,"%30.15f\n",f_val);
fclose(output_out);

}
