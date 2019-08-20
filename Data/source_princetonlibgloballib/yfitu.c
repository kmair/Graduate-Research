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
	3 /* nvar */,
	1 /* nobj */,
	0 /* ncon */,
	0 /* nzc */,
	0 /* densejac */,

	/* objtype (0 = minimize, 1 = maximize) */

	0 };

 real boundc_[1+6+0] /* Infinity, variable bounds, constraint bounds */ = {
		1.7e80,
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80};

 real x0comn_[3] = {
		0.6,
		-0.6,
		20. };

 real
feval0_(fint *nobj, real *x)
{
	real v[5];


  /***  objective ***/

	v[0] = tan(x[0]);
	if (errno) in_trouble("tan",x[0]);
	v[1] = x[2] * v[0];
	v[0] = -21.158931 + v[1];
	v[1] = tan(x[0]);
	if (errno) in_trouble("tan",x[0]);
	v[2] = x[2] * v[1];
	v[1] = -21.158931 + v[2];
	v[2] = v[0] * v[1];
	v[0] = 0.9375 * x[0];
	v[1] = 0.0625 * x[1];
	v[3] = v[0] + v[1];
	v[0] = tan(v[3]);
	if (errno) in_trouble("tan",v[3]);
	v[3] = x[2] * v[0];
	v[0] = -17.591719 + v[3];
	v[3] = 0.9375 * x[0];
	v[1] = 0.0625 * x[1];
	v[4] = v[3] + v[1];
	v[3] = tan(v[4]);
	if (errno) in_trouble("tan",v[4]);
	v[4] = x[2] * v[3];
	v[3] = -17.591719 + v[4];
	v[4] = v[0] * v[3];
	v[2] += v[4];
	v[4] = 0.875 * x[0];
	v[0] = 0.125 * x[1];
	v[3] = v[4] + v[0];
	v[4] = tan(v[3]);
	if (errno) in_trouble("tan",v[3]);
	v[3] = x[2] * v[4];
	v[4] = -14.046854 + v[3];
	v[3] = 0.875 * x[0];
	v[0] = 0.125 * x[1];
	v[1] = v[3] + v[0];
	v[3] = tan(v[1]);
	if (errno) in_trouble("tan",v[1]);
	v[1] = x[2] * v[3];
	v[3] = -14.046854 + v[1];
	v[1] = v[4] * v[3];
	v[2] += v[1];
	v[1] = 0.8125 * x[0];
	v[4] = 0.1875 * x[1];
	v[3] = v[1] + v[4];
	v[1] = tan(v[3]);
	if (errno) in_trouble("tan",v[3]);
	v[3] = x[2] * v[1];
	v[1] = -10.519732 + v[3];
	v[3] = 0.8125 * x[0];
	v[4] = 0.1875 * x[1];
	v[0] = v[3] + v[4];
	v[3] = tan(v[0]);
	if (errno) in_trouble("tan",v[0]);
	v[0] = x[2] * v[3];
	v[3] = -10.519732 + v[0];
	v[0] = v[1] * v[3];
	v[2] += v[0];
	v[0] = 0.75 * x[0];
	v[1] = 0.25 * x[1];
	v[3] = v[0] + v[1];
	v[0] = tan(v[3]);
	if (errno) in_trouble("tan",v[3]);
	v[3] = x[2] * v[0];
	v[0] = -7.0058392 + v[3];
	v[3] = 0.75 * x[0];
	v[1] = 0.25 * x[1];
	v[4] = v[3] + v[1];
	v[3] = tan(v[4]);
	if (errno) in_trouble("tan",v[4]);
	v[4] = x[2] * v[3];
	v[3] = -7.0058392 + v[4];
	v[4] = v[0] * v[3];
	v[2] += v[4];
	v[4] = 0.6875 * x[0];
	v[0] = 0.3125 * x[1];
	v[3] = v[4] + v[0];
	v[4] = tan(v[3]);
	if (errno) in_trouble("tan",v[3]);
	v[3] = x[2] * v[4];
	v[4] = -3.5007293 + v[3];
	v[3] = 0.6875 * x[0];
	v[0] = 0.3125 * x[1];
	v[1] = v[3] + v[0];
	v[3] = tan(v[1]);
	if (errno) in_trouble("tan",v[1]);
	v[1] = x[2] * v[3];
	v[3] = -3.5007293 + v[1];
	v[1] = v[4] * v[3];
	v[2] += v[1];
	v[1] = 0.625 * x[0];
	v[4] = 0.375 * x[1];
	v[3] = v[1] + v[4];
	v[1] = tan(v[3]);
	if (errno) in_trouble("tan",v[3]);
	v[3] = x[2] * v[1];
	v[1] = 0.625 * x[0];
	v[4] = 0.375 * x[1];
	v[0] = v[1] + v[4];
	v[1] = tan(v[0]);
	if (errno) in_trouble("tan",v[0]);
	v[0] = x[2] * v[1];
	v[1] = v[3] * v[0];
	v[2] += v[1];
	v[1] = 0.5625 * x[0];
	v[3] = 0.4375 * x[1];
	v[0] = v[1] + v[3];
	v[1] = tan(v[0]);
	if (errno) in_trouble("tan",v[0]);
	v[0] = x[2] * v[1];
	v[1] = 3.5007293 + v[0];
	v[0] = 0.5625 * x[0];
	v[3] = 0.4375 * x[1];
	v[4] = v[0] + v[3];
	v[0] = tan(v[4]);
	if (errno) in_trouble("tan",v[4]);
	v[4] = x[2] * v[0];
	v[0] = 3.5007293 + v[4];
	v[4] = v[1] * v[0];
	v[2] += v[4];
	v[4] = 0.5 * x[0];
	v[1] = 0.5 * x[1];
	v[0] = v[4] + v[1];
	v[4] = tan(v[0]);
	if (errno) in_trouble("tan",v[0]);
	v[0] = x[2] * v[4];
	v[4] = 7.0058392 + v[0];
	v[0] = 0.5 * x[0];
	v[1] = 0.5 * x[1];
	v[3] = v[0] + v[1];
	v[0] = tan(v[3]);
	if (errno) in_trouble("tan",v[3]);
	v[3] = x[2] * v[0];
	v[0] = 7.0058392 + v[3];
	v[3] = v[4] * v[0];
	v[2] += v[3];
	v[3] = 0.4375 * x[0];
	v[4] = 0.5625 * x[1];
	v[0] = v[3] + v[4];
	v[3] = tan(v[0]);
	if (errno) in_trouble("tan",v[0]);
	v[0] = x[2] * v[3];
	v[3] = 10.519732 + v[0];
	v[0] = 0.4375 * x[0];
	v[4] = 0.5625 * x[1];
	v[1] = v[0] + v[4];
	v[0] = tan(v[1]);
	if (errno) in_trouble("tan",v[1]);
	v[1] = x[2] * v[0];
	v[0] = 10.519732 + v[1];
	v[1] = v[3] * v[0];
	v[2] += v[1];
	v[1] = 0.375 * x[0];
	v[3] = 0.625 * x[1];
	v[0] = v[1] + v[3];
	v[1] = tan(v[0]);
	if (errno) in_trouble("tan",v[0]);
	v[0] = x[2] * v[1];
	v[1] = 14.046854 + v[0];
	v[0] = 0.375 * x[0];
	v[3] = 0.625 * x[1];
	v[4] = v[0] + v[3];
	v[0] = tan(v[4]);
	if (errno) in_trouble("tan",v[4]);
	v[4] = x[2] * v[0];
	v[0] = 14.046854 + v[4];
	v[4] = v[1] * v[0];
	v[2] += v[4];
	v[4] = 0.3125 * x[0];
	v[1] = 0.6875 * x[1];
	v[0] = v[4] + v[1];
	v[4] = tan(v[0]);
	if (errno) in_trouble("tan",v[0]);
	v[0] = x[2] * v[4];
	v[4] = 17.591719 + v[0];
	v[0] = 0.3125 * x[0];
	v[1] = 0.6875 * x[1];
	v[3] = v[0] + v[1];
	v[0] = tan(v[3]);
	if (errno) in_trouble("tan",v[3]);
	v[3] = x[2] * v[0];
	v[0] = 17.591719 + v[3];
	v[3] = v[4] * v[0];
	v[2] += v[3];
	v[3] = 0.25 * x[0];
	v[4] = 0.75 * x[1];
	v[0] = v[3] + v[4];
	v[3] = tan(v[0]);
	if (errno) in_trouble("tan",v[0]);
	v[0] = x[2] * v[3];
	v[3] = 21.158931 + v[0];
	v[0] = 0.25 * x[0];
	v[4] = 0.75 * x[1];
	v[1] = v[0] + v[4];
	v[0] = tan(v[1]);
	if (errno) in_trouble("tan",v[1]);
	v[1] = x[2] * v[0];
	v[0] = 21.158931 + v[1];
	v[1] = v[3] * v[0];
	v[2] += v[1];
	v[1] = 0.1875 * x[0];
	v[3] = 0.8125 * x[1];
	v[0] = v[1] + v[3];
	v[1] = tan(v[0]);
	if (errno) in_trouble("tan",v[0]);
	v[0] = x[2] * v[1];
	v[1] = 24.753206 + v[0];
	v[0] = 0.1875 * x[0];
	v[3] = 0.8125 * x[1];
	v[4] = v[0] + v[3];
	v[0] = tan(v[4]);
	if (errno) in_trouble("tan",v[4]);
	v[4] = x[2] * v[0];
	v[0] = 24.753206 + v[4];
	v[4] = v[1] * v[0];
	v[2] += v[4];
	v[4] = 0.125 * x[0];
	v[1] = 0.875 * x[1];
	v[0] = v[4] + v[1];
	v[4] = tan(v[0]);
	if (errno) in_trouble("tan",v[0]);
	v[0] = x[2] * v[4];
	v[4] = 28.379405 + v[0];
	v[0] = 0.125 * x[0];
	v[1] = 0.875 * x[1];
	v[3] = v[0] + v[1];
	v[0] = tan(v[3]);
	if (errno) in_trouble("tan",v[3]);
	v[3] = x[2] * v[0];
	v[0] = 28.379405 + v[3];
	v[3] = v[4] * v[0];
	v[2] += v[3];
	v[3] = 0.0625 * x[0];
	v[4] = 0.9375 * x[1];
	v[0] = v[3] + v[4];
	v[3] = tan(v[0]);
	if (errno) in_trouble("tan",v[0]);
	v[0] = x[2] * v[3];
	v[3] = 32.042552 + v[0];
	v[0] = 0.0625 * x[0];
	v[4] = 0.9375 * x[1];
	v[1] = v[0] + v[4];
	v[0] = tan(v[1]);
	if (errno) in_trouble("tan",v[1]);
	v[1] = x[2] * v[0];
	v[0] = 32.042552 + v[1];
	v[1] = v[3] * v[0];
	v[2] += v[1];
	v[1] = tan(x[1]);
	if (errno) in_trouble("tan",x[1]);
	v[3] = x[2] * v[1];
	v[1] = 35.747869 + v[3];
	v[3] = tan(x[1]);
	if (errno) in_trouble("tan",x[1]);
	v[0] = x[2] * v[3];
	v[3] = 35.747869 + v[0];
	v[0] = v[1] * v[3];
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

x_input = malloc (3 * sizeof(real));
input_values = malloc (3 * sizeof(double));
c_val = malloc (0 * sizeof(real));

file_input = fopen("input.in","r");
for (i=0; i < 3; i++)
    fscanf(file_input, "%lf" ,&input_values[i]);

fclose(file_input);
for (i=0; i < 3; i++)
 {
    x_input[i] = input_values[i];
 }

f_val = feval0_(&objective_number, x_input);

FILE *output_out;
output_out = fopen ("output.out","w");
fprintf(output_out,"%30.15f\n",f_val);
fclose(output_out);

}
