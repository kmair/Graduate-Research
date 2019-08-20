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
		0.,
		10.,
		0.,
		10.,
		0.,
		10.};

 real x0comn_[3] = {
		1.,
		2.,
		1. };

 real
feval0_(fint *nobj, real *x)
{
	real v[4];


  /***  objective ***/

	v[0] = 0.1 * x[0];
	v[1] = -v[0];
	v[0] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = 0.1 * x[1];
	v[2] = -v[1];
	v[1] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = x[2] * v[1];
	v[1] = v[0] - v[2];
	v[0] = 0.9345597878212522 + v[1];
	v[1] = v[0] * v[0];
	v[0] = 0.2 * x[0];
	v[2] = -v[0];
	v[0] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = 0.2 * x[1];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[2];
	v[2] = v[0] - v[3];
	v[0] = -0.1420543368949183 + v[2];
	v[2] = v[0] * v[0];
	v[1] += v[2];
	v[2] = 0.30000000000000004 * x[0];
	v[0] = -v[2];
	v[2] = exp(v[0]);
	if (errno) in_trouble("exp",v[0]);
	v[0] = 0.30000000000000004 * x[1];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[0];
	v[0] = v[2] - v[3];
	v[2] = -0.49188287884239823 + v[0];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 0.4 * x[0];
	v[2] = -v[0];
	v[0] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = 0.4 * x[1];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[2];
	v[2] = v[0] - v[3];
	v[0] = -0.5787418515919684 + v[2];
	v[2] = v[0] * v[0];
	v[1] += v[2];
	v[2] = 0.5 * x[0];
	v[0] = -v[2];
	v[2] = exp(v[0]);
	if (errno) in_trouble("exp",v[0]);
	v[0] = 0.5 * x[1];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[0];
	v[0] = v[2] - v[3];
	v[2] = -0.5728409247172062 + v[0];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 0.6000000000000001 * x[0];
	v[2] = -v[0];
	v[0] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = 0.6000000000000001 * x[1];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[2];
	v[2] = v[0] - v[3];
	v[0] = -0.5364178752106946 + v[2];
	v[2] = v[0] * v[0];
	v[1] += v[2];
	v[2] = 0.7000000000000001 * x[0];
	v[0] = -v[2];
	v[2] = exp(v[0]);
	if (errno) in_trouble("exp",v[0]);
	v[0] = 0.7000000000000001 * x[1];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[0];
	v[0] = v[2] - v[3];
	v[2] = -0.4920258939636369 + v[0];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 0.8 * x[0];
	v[2] = -v[0];
	v[0] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = 0.8 * x[1];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[2];
	v[2] = v[0] - v[3];
	v[0] = -0.447651650977709 + v[2];
	v[2] = v[0] * v[0];
	v[1] += v[2];
	v[2] = 0.9 * x[0];
	v[0] = -v[2];
	v[2] = exp(v[0]);
	if (errno) in_trouble("exp",v[0]);
	v[0] = 0.9 * x[1];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[0];
	v[0] = v[2] - v[3];
	v[2] = -0.4059526107201657 + v[0];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = -x[0];
	v[2] = exp(v[0]);
	if (errno) in_trouble("exp",v[0]);
	v[0] = -x[1];
	v[3] = exp(v[0]);
	if (errno) in_trouble("exp",v[0]);
	v[0] = x[2] * v[3];
	v[3] = v[2] - v[0];
	v[2] = -0.36765244152262994 + v[3];
	v[3] = v[2] * v[2];
	v[1] += v[3];

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
