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
		-150.,
		150.,
		-150.,
		150.,
		-1.7e80,
		1.7e80};

 real x0comn_[3] = {
		0.,
		10.,
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
	v[2] = v[0] - v[1];
	v[0] = -0.9048374180359595 * x[2];
	v[2] += v[0];
	v[0] = 0.36787944117144233 * x[2];
	v[2] += v[0];
	v[0] = v[2] * v[2];
	v[2] = 0.2 * x[0];
	v[1] = -v[2];
	v[2] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = 0.2 * x[1];
	v[3] = -v[1];
	v[1] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = v[2] - v[1];
	v[2] = -0.8187307530779818 * x[2];
	v[3] += v[2];
	v[2] = 0.1353352832366127 * x[2];
	v[3] += v[2];
	v[2] = v[3] * v[3];
	v[0] += v[2];
	v[2] = 0.30000000000000004 * x[0];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = 0.30000000000000004 * x[1];
	v[1] = -v[3];
	v[3] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = v[2] - v[3];
	v[2] = -0.7408182206817179 * x[2];
	v[1] += v[2];
	v[2] = 0.04978706836786393 * x[2];
	v[1] += v[2];
	v[2] = v[1] * v[1];
	v[0] += v[2];
	v[2] = 0.4 * x[0];
	v[1] = -v[2];
	v[2] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = 0.4 * x[1];
	v[3] = -v[1];
	v[1] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = v[2] - v[1];
	v[2] = -0.6703200460356393 * x[2];
	v[3] += v[2];
	v[2] = 0.018315638888734182 * x[2];
	v[3] += v[2];
	v[2] = v[3] * v[3];
	v[0] += v[2];
	v[2] = 0.5 * x[0];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = 0.5 * x[1];
	v[1] = -v[3];
	v[3] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = v[2] - v[3];
	v[2] = -0.6065306597126334 * x[2];
	v[1] += v[2];
	v[2] = 0.006737946999085465 * x[2];
	v[1] += v[2];
	v[2] = v[1] * v[1];
	v[0] += v[2];
	v[2] = 0.6000000000000001 * x[0];
	v[1] = -v[2];
	v[2] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = 0.6000000000000001 * x[1];
	v[3] = -v[1];
	v[1] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = v[2] - v[1];
	v[2] = -0.5488116360940264 * x[2];
	v[3] += v[2];
	v[2] = 0.002478752176666357 * x[2];
	v[3] += v[2];
	v[2] = v[3] * v[3];
	v[0] += v[2];
	v[2] = 0.7000000000000001 * x[0];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = 0.7000000000000001 * x[1];
	v[1] = -v[3];
	v[3] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = v[2] - v[3];
	v[2] = -0.49658530379140947 * x[2];
	v[1] += v[2];
	v[2] = 0.0009118819655545155 * x[2];
	v[1] += v[2];
	v[2] = v[1] * v[1];
	v[0] += v[2];
	v[2] = 0.8 * x[0];
	v[1] = -v[2];
	v[2] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = 0.8 * x[1];
	v[3] = -v[1];
	v[1] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = v[2] - v[1];
	v[2] = -0.44932896411722156 * x[2];
	v[3] += v[2];
	v[2] = 0.00033546262790251185 * x[2];
	v[3] += v[2];
	v[2] = v[3] * v[3];
	v[0] += v[2];
	v[2] = 0.9 * x[0];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = 0.9 * x[1];
	v[1] = -v[3];
	v[3] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = v[2] - v[3];
	v[2] = -0.4065696597405991 * x[2];
	v[1] += v[2];
	v[2] = 0.00012340980408667953 * x[2];
	v[1] += v[2];
	v[2] = v[1] * v[1];
	v[0] += v[2];
	v[2] = -x[0];
	v[1] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = -x[1];
	v[3] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = v[1] - v[3];
	v[1] = -0.36787944117144233 * x[2];
	v[2] += v[1];
	v[1] = 4.539992976248483e-05 * x[2];
	v[2] += v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];

	return v[0];
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
