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
		-0.5,
		0.5,
		1.5,
		2.5,
		-2.,
		-1.,
		0.001,
		0.1,
		0.001,
		0.1};

 real x0comn_[5] = {
		0.5,
		1.5,
		-1.,
		0.01,
		0.02 };

 real
feval0_(fint *nobj, real *x)
{
	real v[4];


  /***  objective ***/

	v[0] = x[0] + x[1];
	v[0] += x[2];
	v[1] = 0.844 - v[0];
	v[0] = v[1] * v[1];
	v[1] = 10. * x[3];
	v[2] = -v[1];
	v[1] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = x[1] * v[1];
	v[2] += x[0];
	v[1] = 10. * x[4];
	v[3] = -v[1];
	v[1] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[1];
	v[2] += v[3];
	v[3] = 0.908 - v[2];
	v[2] = v[3] * v[3];
	v[0] += v[2];
	v[2] = 20. * x[3];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[2];
	v[3] += x[0];
	v[2] = 20. * x[4];
	v[1] = -v[2];
	v[2] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = x[2] * v[2];
	v[3] += v[1];
	v[1] = 0.932 - v[3];
	v[3] = v[1] * v[1];
	v[0] += v[3];
	v[3] = 30. * x[3];
	v[1] = -v[3];
	v[3] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = x[1] * v[3];
	v[1] += x[0];
	v[3] = 30. * x[4];
	v[2] = -v[3];
	v[3] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = x[2] * v[3];
	v[1] += v[2];
	v[2] = 0.936 - v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = 40. * x[3];
	v[2] = -v[1];
	v[1] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = x[1] * v[1];
	v[2] += x[0];
	v[1] = 40. * x[4];
	v[3] = -v[1];
	v[1] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[1];
	v[2] += v[3];
	v[3] = 0.925 - v[2];
	v[2] = v[3] * v[3];
	v[0] += v[2];
	v[2] = 50. * x[3];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[2];
	v[3] += x[0];
	v[2] = 50. * x[4];
	v[1] = -v[2];
	v[2] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = x[2] * v[2];
	v[3] += v[1];
	v[1] = 0.908 - v[3];
	v[3] = v[1] * v[1];
	v[0] += v[3];
	v[3] = 60. * x[3];
	v[1] = -v[3];
	v[3] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = x[1] * v[3];
	v[1] += x[0];
	v[3] = 60. * x[4];
	v[2] = -v[3];
	v[3] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = x[2] * v[3];
	v[1] += v[2];
	v[2] = 0.881 - v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = 70. * x[3];
	v[2] = -v[1];
	v[1] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = x[1] * v[1];
	v[2] += x[0];
	v[1] = 70. * x[4];
	v[3] = -v[1];
	v[1] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[1];
	v[2] += v[3];
	v[3] = 0.85 - v[2];
	v[2] = v[3] * v[3];
	v[0] += v[2];
	v[2] = 80. * x[3];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[2];
	v[3] += x[0];
	v[2] = 80. * x[4];
	v[1] = -v[2];
	v[2] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = x[2] * v[2];
	v[3] += v[1];
	v[1] = 0.818 - v[3];
	v[3] = v[1] * v[1];
	v[0] += v[3];
	v[3] = 90. * x[3];
	v[1] = -v[3];
	v[3] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = x[1] * v[3];
	v[1] += x[0];
	v[3] = 90. * x[4];
	v[2] = -v[3];
	v[3] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = x[2] * v[3];
	v[1] += v[2];
	v[2] = 0.784 - v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = 100. * x[3];
	v[2] = -v[1];
	v[1] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = x[1] * v[1];
	v[2] += x[0];
	v[1] = 100. * x[4];
	v[3] = -v[1];
	v[1] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[1];
	v[2] += v[3];
	v[3] = 0.751 - v[2];
	v[2] = v[3] * v[3];
	v[0] += v[2];
	v[2] = 110. * x[3];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[2];
	v[3] += x[0];
	v[2] = 110. * x[4];
	v[1] = -v[2];
	v[2] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = x[2] * v[2];
	v[3] += v[1];
	v[1] = 0.718 - v[3];
	v[3] = v[1] * v[1];
	v[0] += v[3];
	v[3] = 120. * x[3];
	v[1] = -v[3];
	v[3] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = x[1] * v[3];
	v[1] += x[0];
	v[3] = 120. * x[4];
	v[2] = -v[3];
	v[3] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = x[2] * v[3];
	v[1] += v[2];
	v[2] = 0.685 - v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = 130. * x[3];
	v[2] = -v[1];
	v[1] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = x[1] * v[1];
	v[2] += x[0];
	v[1] = 130. * x[4];
	v[3] = -v[1];
	v[1] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[1];
	v[2] += v[3];
	v[3] = 0.658 - v[2];
	v[2] = v[3] * v[3];
	v[0] += v[2];
	v[2] = 140. * x[3];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[2];
	v[3] += x[0];
	v[2] = 140. * x[4];
	v[1] = -v[2];
	v[2] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = x[2] * v[2];
	v[3] += v[1];
	v[1] = 0.628 - v[3];
	v[3] = v[1] * v[1];
	v[0] += v[3];
	v[3] = 150. * x[3];
	v[1] = -v[3];
	v[3] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = x[1] * v[3];
	v[1] += x[0];
	v[3] = 150. * x[4];
	v[2] = -v[3];
	v[3] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = x[2] * v[3];
	v[1] += v[2];
	v[2] = 0.603 - v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = 160. * x[3];
	v[2] = -v[1];
	v[1] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = x[1] * v[1];
	v[2] += x[0];
	v[1] = 160. * x[4];
	v[3] = -v[1];
	v[1] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[1];
	v[2] += v[3];
	v[3] = 0.58 - v[2];
	v[2] = v[3] * v[3];
	v[0] += v[2];
	v[2] = 170. * x[3];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[2];
	v[3] += x[0];
	v[2] = 170. * x[4];
	v[1] = -v[2];
	v[2] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = x[2] * v[2];
	v[3] += v[1];
	v[1] = 0.558 - v[3];
	v[3] = v[1] * v[1];
	v[0] += v[3];
	v[3] = 180. * x[3];
	v[1] = -v[3];
	v[3] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = x[1] * v[3];
	v[1] += x[0];
	v[3] = 180. * x[4];
	v[2] = -v[3];
	v[3] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = x[2] * v[3];
	v[1] += v[2];
	v[2] = 0.538 - v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = 190. * x[3];
	v[2] = -v[1];
	v[1] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = x[1] * v[1];
	v[2] += x[0];
	v[1] = 190. * x[4];
	v[3] = -v[1];
	v[1] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[1];
	v[2] += v[3];
	v[3] = 0.522 - v[2];
	v[2] = v[3] * v[3];
	v[0] += v[2];
	v[2] = 200. * x[3];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[2];
	v[3] += x[0];
	v[2] = 200. * x[4];
	v[1] = -v[2];
	v[2] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = x[2] * v[2];
	v[3] += v[1];
	v[1] = 0.506 - v[3];
	v[3] = v[1] * v[1];
	v[0] += v[3];
	v[3] = 210. * x[3];
	v[1] = -v[3];
	v[3] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = x[1] * v[3];
	v[1] += x[0];
	v[3] = 210. * x[4];
	v[2] = -v[3];
	v[3] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = x[2] * v[3];
	v[1] += v[2];
	v[2] = 0.49 - v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = 220. * x[3];
	v[2] = -v[1];
	v[1] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = x[1] * v[1];
	v[2] += x[0];
	v[1] = 220. * x[4];
	v[3] = -v[1];
	v[1] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[1];
	v[2] += v[3];
	v[3] = 0.478 - v[2];
	v[2] = v[3] * v[3];
	v[0] += v[2];
	v[2] = 230. * x[3];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[2];
	v[3] += x[0];
	v[2] = 230. * x[4];
	v[1] = -v[2];
	v[2] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = x[2] * v[2];
	v[3] += v[1];
	v[1] = 0.467 - v[3];
	v[3] = v[1] * v[1];
	v[0] += v[3];
	v[3] = 240. * x[3];
	v[1] = -v[3];
	v[3] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = x[1] * v[3];
	v[1] += x[0];
	v[3] = 240. * x[4];
	v[2] = -v[3];
	v[3] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = x[2] * v[3];
	v[1] += v[2];
	v[2] = 0.457 - v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = 250. * x[3];
	v[2] = -v[1];
	v[1] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = x[1] * v[1];
	v[2] += x[0];
	v[1] = 250. * x[4];
	v[3] = -v[1];
	v[1] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[1];
	v[2] += v[3];
	v[3] = 0.448 - v[2];
	v[2] = v[3] * v[3];
	v[0] += v[2];
	v[2] = 260. * x[3];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[2];
	v[3] += x[0];
	v[2] = 260. * x[4];
	v[1] = -v[2];
	v[2] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = x[2] * v[2];
	v[3] += v[1];
	v[1] = 0.438 - v[3];
	v[3] = v[1] * v[1];
	v[0] += v[3];
	v[3] = 270. * x[3];
	v[1] = -v[3];
	v[3] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = x[1] * v[3];
	v[1] += x[0];
	v[3] = 270. * x[4];
	v[2] = -v[3];
	v[3] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = x[2] * v[3];
	v[1] += v[2];
	v[2] = 0.431 - v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = 280. * x[3];
	v[2] = -v[1];
	v[1] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = x[1] * v[1];
	v[2] += x[0];
	v[1] = 280. * x[4];
	v[3] = -v[1];
	v[1] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[1];
	v[2] += v[3];
	v[3] = 0.424 - v[2];
	v[2] = v[3] * v[3];
	v[0] += v[2];
	v[2] = 290. * x[3];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[2];
	v[3] += x[0];
	v[2] = 290. * x[4];
	v[1] = -v[2];
	v[2] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = x[2] * v[2];
	v[3] += v[1];
	v[1] = 0.42 - v[3];
	v[3] = v[1] * v[1];
	v[0] += v[3];
	v[3] = 300. * x[3];
	v[1] = -v[3];
	v[3] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = x[1] * v[3];
	v[1] += x[0];
	v[3] = 300. * x[4];
	v[2] = -v[3];
	v[3] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = x[2] * v[3];
	v[1] += v[2];
	v[2] = 0.414 - v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = 310. * x[3];
	v[2] = -v[1];
	v[1] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = x[1] * v[1];
	v[2] += x[0];
	v[1] = 310. * x[4];
	v[3] = -v[1];
	v[1] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[1];
	v[2] += v[3];
	v[3] = 0.411 - v[2];
	v[2] = v[3] * v[3];
	v[0] += v[2];
	v[2] = 320. * x[3];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[2];
	v[3] += x[0];
	v[2] = 320. * x[4];
	v[1] = -v[2];
	v[2] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = x[2] * v[2];
	v[3] += v[1];
	v[1] = 0.406 - v[3];
	v[3] = v[1] * v[1];
	v[0] += v[3];

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
