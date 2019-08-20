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
		-50.,
		50.,
		-50.,
		50.,
		-1.7e80,
		1.7e80};

 real x0comn_[3] = {
		0.,
		10.,
		20. };

 real
feval0_(fint *nobj, real *x)
{
	real v[4];


  /***  objective ***/

	v[0] = x[0] / 10.;
	v[1] = -v[0];
	v[0] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = x[1] / 10.;
	v[2] = -v[1];
	v[1] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = v[0] - v[1];
	v[0] = -0.5369579768645172 * x[2];
	v[1] = v[2] + v[0];
	v[2] = v[1] * v[1];
	v[1] = -0.2 * x[0];
	v[0] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = -0.2 * x[1];
	v[3] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = v[0] - v[3];
	v[0] = -0.6833954698413691 * x[2];
	v[3] = v[1] + v[0];
	v[1] = v[3] * v[3];
	v[2] += v[1];
	v[1] = -0.3 * x[0];
	v[3] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = -0.3 * x[1];
	v[0] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = v[3] - v[0];
	v[3] = -0.6910311523138539 * x[2];
	v[0] = v[1] + v[3];
	v[1] = v[0] * v[0];
	v[2] += v[1];
	v[1] = -0.4 * x[0];
	v[0] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = -0.4 * x[1];
	v[3] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = v[0] - v[3];
	v[0] = -0.6520044071469051 * x[2];
	v[3] = v[1] + v[0];
	v[1] = v[3] * v[3];
	v[2] += v[1];
	v[1] = -0.5 * x[0];
	v[3] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = -0.5 * x[1];
	v[0] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = v[3] - v[0];
	v[3] = -0.599792712713548 * x[2];
	v[0] = v[1] + v[3];
	v[1] = v[0] * v[0];
	v[2] += v[1];
	v[1] = -0.6 * x[0];
	v[0] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = -0.6 * x[1];
	v[3] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = v[0] - v[3];
	v[0] = -0.5463328839173601 * x[2];
	v[3] = v[1] + v[0];
	v[1] = v[3] * v[3];
	v[2] += v[1];
	v[1] = -0.7 * x[0];
	v[3] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = -0.7 * x[1];
	v[0] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = v[3] - v[0];
	v[3] = -0.49567342182585505 * x[2];
	v[0] = v[1] + v[3];
	v[1] = v[0] * v[0];
	v[2] += v[1];
	v[1] = -0.8 * x[0];
	v[0] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = -0.8 * x[1];
	v[3] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = v[0] - v[3];
	v[0] = -0.44899350148931905 * x[2];
	v[3] = v[1] + v[0];
	v[1] = v[3] * v[3];
	v[2] += v[1];
	v[1] = -0.9 * x[0];
	v[3] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = -0.9 * x[1];
	v[0] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = v[3] - v[0];
	v[3] = -0.4064462499365124 * x[2];
	v[0] = v[1] + v[3];
	v[1] = v[0] * v[0];
	v[2] += v[1];
	v[1] = -1. * x[0];
	v[0] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = -1. * x[1];
	v[3] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = v[0] - v[3];
	v[0] = -0.36783404124167984 * x[2];
	v[3] = v[1] + v[0];
	v[1] = v[3] * v[3];
	v[2] += v[1];

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
