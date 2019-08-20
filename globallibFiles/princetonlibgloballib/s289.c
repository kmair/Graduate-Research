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
	30 /* nvar */,
	1 /* nobj */,
	0 /* ncon */,
	0 /* nzc */,
	0 /* densejac */,

	/* objtype (0 = minimize, 1 = maximize) */

	0 };

 real boundc_[1+60+0] /* Infinity, variable bounds, constraint bounds */ = {
		1.7e80,
		-15.,
		15.,
		-15.,
		15.,
		-15.,
		15.,
		-15.,
		15.,
		-15.,
		15.,
		-15.,
		15.,
		-15.,
		15.,
		-15.,
		15.,
		-15.,
		15.,
		-15.,
		15.,
		-15.,
		15.,
		-15.,
		15.,
		-15.,
		15.,
		-15.,
		15.,
		-15.,
		15.,
		-15.,
		15.,
		-15.,
		15.,
		-15.,
		15.,
		-15.,
		15.,
		-15.,
		15.,
		-15.,
		15.,
		-15.,
		15.,
		-15.,
		15.,
		-15.,
		15.,
		-15.,
		15.,
		-15.,
		15.,
		-15.,
		15.,
		-15.,
		15.,
		-15.,
		15.,
		-15.,
		15.};

 real x0comn_[30] = {
		-1.03,
		1.07,
		-1.1,
		1.13,
		-1.17,
		1.2,
		-1.23,
		1.27,
		-1.3,
		1.33,
		-1.37,
		1.4,
		-1.43,
		1.47,
		-1.5,
		1.53,
		-1.57,
		1.6,
		-1.63,
		1.67,
		-1.7,
		1.73,
		-1.77,
		1.8,
		-1.83,
		1.87,
		-1.9,
		1.93,
		-1.97,
		2. };

 real
feval0_(fint *nobj, real *x)
{
	real v[3];


  /***  objective ***/

	v[0] = x[0] * x[0];
	v[1] = v[0] / 60.;
	v[0] = x[1] * x[1];
	v[2] = v[0] / 60.;
	v[1] += v[2];
	v[2] = x[2] * x[2];
	v[0] = v[2] / 60.;
	v[1] += v[0];
	v[0] = x[3] * x[3];
	v[2] = v[0] / 60.;
	v[1] += v[2];
	v[2] = x[4] * x[4];
	v[0] = v[2] / 60.;
	v[1] += v[0];
	v[0] = x[5] * x[5];
	v[2] = v[0] / 60.;
	v[1] += v[2];
	v[2] = x[6] * x[6];
	v[0] = v[2] / 60.;
	v[1] += v[0];
	v[0] = x[7] * x[7];
	v[2] = v[0] / 60.;
	v[1] += v[2];
	v[2] = x[8] * x[8];
	v[0] = v[2] / 60.;
	v[1] += v[0];
	v[0] = x[9] * x[9];
	v[2] = v[0] / 60.;
	v[1] += v[2];
	v[2] = x[10] * x[10];
	v[0] = v[2] / 60.;
	v[1] += v[0];
	v[0] = x[11] * x[11];
	v[2] = v[0] / 60.;
	v[1] += v[2];
	v[2] = x[12] * x[12];
	v[0] = v[2] / 60.;
	v[1] += v[0];
	v[0] = x[13] * x[13];
	v[2] = v[0] / 60.;
	v[1] += v[2];
	v[2] = x[14] * x[14];
	v[0] = v[2] / 60.;
	v[1] += v[0];
	v[0] = x[15] * x[15];
	v[2] = v[0] / 60.;
	v[1] += v[2];
	v[2] = x[16] * x[16];
	v[0] = v[2] / 60.;
	v[1] += v[0];
	v[0] = x[17] * x[17];
	v[2] = v[0] / 60.;
	v[1] += v[2];
	v[2] = x[18] * x[18];
	v[0] = v[2] / 60.;
	v[1] += v[0];
	v[0] = x[19] * x[19];
	v[2] = v[0] / 60.;
	v[1] += v[2];
	v[2] = x[20] * x[20];
	v[0] = v[2] / 60.;
	v[1] += v[0];
	v[0] = x[21] * x[21];
	v[2] = v[0] / 60.;
	v[1] += v[2];
	v[2] = x[22] * x[22];
	v[0] = v[2] / 60.;
	v[1] += v[0];
	v[0] = x[23] * x[23];
	v[2] = v[0] / 60.;
	v[1] += v[2];
	v[2] = x[24] * x[24];
	v[0] = v[2] / 60.;
	v[1] += v[0];
	v[0] = x[25] * x[25];
	v[2] = v[0] / 60.;
	v[1] += v[2];
	v[2] = x[26] * x[26];
	v[0] = v[2] / 60.;
	v[1] += v[0];
	v[0] = x[27] * x[27];
	v[2] = v[0] / 60.;
	v[1] += v[2];
	v[2] = x[28] * x[28];
	v[0] = v[2] / 60.;
	v[1] += v[0];
	v[0] = x[29] * x[29];
	v[2] = v[0] / 60.;
	v[1] += v[2];
	v[2] = -v[1];
	v[1] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = 1. - v[1];

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

x_input = malloc (30 * sizeof(real));
input_values = malloc (30 * sizeof(double));
c_val = malloc (0 * sizeof(real));

file_input = fopen("input.in","r");
for (i=0; i < 30; i++)
    fscanf(file_input, "%lf" ,&input_values[i]);

fclose(file_input);
for (i=0; i < 30; i++)
 {
    x_input[i] = input_values[i];
 }

f_val = feval0_(&objective_number, x_input);

FILE *output_out;
output_out = fopen ("output.out","w");
fprintf(output_out,"%30.15f\n",f_val);
fclose(output_out);

}
