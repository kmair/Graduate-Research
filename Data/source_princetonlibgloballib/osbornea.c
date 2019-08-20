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
		-0.1,
		0.1,
		-0.1,
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

	v[0] = 0.844 - x[0];
	v[1] = v[0] - x[1];
	v[0] = v[1] - x[2];
	v[1] = v[0] * v[0];
	v[0] = 0.908 - x[0];
	v[2] = 10. * x[3];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[2];
	v[2] = v[0] - v[3];
	v[0] = 10. * x[4];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[0];
	v[0] = v[2] - v[3];
	v[2] = v[0] * v[0];
	v[1] += v[2];
	v[2] = 0.932 - x[0];
	v[0] = 20. * x[3];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[0];
	v[0] = v[2] - v[3];
	v[2] = 20. * x[4];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[2];
	v[2] = v[0] - v[3];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 0.936 - x[0];
	v[2] = 30. * x[3];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[2];
	v[2] = v[0] - v[3];
	v[0] = 30. * x[4];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[0];
	v[0] = v[2] - v[3];
	v[2] = v[0] * v[0];
	v[1] += v[2];
	v[2] = 0.925 - x[0];
	v[0] = 40. * x[3];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[0];
	v[0] = v[2] - v[3];
	v[2] = 40. * x[4];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[2];
	v[2] = v[0] - v[3];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 0.908 - x[0];
	v[2] = 50. * x[3];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[2];
	v[2] = v[0] - v[3];
	v[0] = 50. * x[4];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[0];
	v[0] = v[2] - v[3];
	v[2] = v[0] * v[0];
	v[1] += v[2];
	v[2] = 0.881 - x[0];
	v[0] = 60. * x[3];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[0];
	v[0] = v[2] - v[3];
	v[2] = 60. * x[4];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[2];
	v[2] = v[0] - v[3];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 0.85 - x[0];
	v[2] = 70. * x[3];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[2];
	v[2] = v[0] - v[3];
	v[0] = 70. * x[4];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[0];
	v[0] = v[2] - v[3];
	v[2] = v[0] * v[0];
	v[1] += v[2];
	v[2] = 0.818 - x[0];
	v[0] = 80. * x[3];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[0];
	v[0] = v[2] - v[3];
	v[2] = 80. * x[4];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[2];
	v[2] = v[0] - v[3];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 0.784 - x[0];
	v[2] = 90. * x[3];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[2];
	v[2] = v[0] - v[3];
	v[0] = 90. * x[4];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[0];
	v[0] = v[2] - v[3];
	v[2] = v[0] * v[0];
	v[1] += v[2];
	v[2] = 0.751 - x[0];
	v[0] = 100. * x[3];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[0];
	v[0] = v[2] - v[3];
	v[2] = 100. * x[4];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[2];
	v[2] = v[0] - v[3];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 0.718 - x[0];
	v[2] = 110. * x[3];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[2];
	v[2] = v[0] - v[3];
	v[0] = 110. * x[4];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[0];
	v[0] = v[2] - v[3];
	v[2] = v[0] * v[0];
	v[1] += v[2];
	v[2] = 0.685 - x[0];
	v[0] = 120. * x[3];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[0];
	v[0] = v[2] - v[3];
	v[2] = 120. * x[4];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[2];
	v[2] = v[0] - v[3];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 0.658 - x[0];
	v[2] = 130. * x[3];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[2];
	v[2] = v[0] - v[3];
	v[0] = 130. * x[4];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[0];
	v[0] = v[2] - v[3];
	v[2] = v[0] * v[0];
	v[1] += v[2];
	v[2] = 0.628 - x[0];
	v[0] = 140. * x[3];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[0];
	v[0] = v[2] - v[3];
	v[2] = 140. * x[4];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[2];
	v[2] = v[0] - v[3];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 0.603 - x[0];
	v[2] = 150. * x[3];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[2];
	v[2] = v[0] - v[3];
	v[0] = 150. * x[4];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[0];
	v[0] = v[2] - v[3];
	v[2] = v[0] * v[0];
	v[1] += v[2];
	v[2] = 0.58 - x[0];
	v[0] = 160. * x[3];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[0];
	v[0] = v[2] - v[3];
	v[2] = 160. * x[4];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[2];
	v[2] = v[0] - v[3];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 0.558 - x[0];
	v[2] = 170. * x[3];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[2];
	v[2] = v[0] - v[3];
	v[0] = 170. * x[4];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[0];
	v[0] = v[2] - v[3];
	v[2] = v[0] * v[0];
	v[1] += v[2];
	v[2] = 0.538 - x[0];
	v[0] = 180. * x[3];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[0];
	v[0] = v[2] - v[3];
	v[2] = 180. * x[4];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[2];
	v[2] = v[0] - v[3];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 0.522 - x[0];
	v[2] = 190. * x[3];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[2];
	v[2] = v[0] - v[3];
	v[0] = 190. * x[4];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[0];
	v[0] = v[2] - v[3];
	v[2] = v[0] * v[0];
	v[1] += v[2];
	v[2] = 0.506 - x[0];
	v[0] = 200. * x[3];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[0];
	v[0] = v[2] - v[3];
	v[2] = 200. * x[4];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[2];
	v[2] = v[0] - v[3];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 0.49 - x[0];
	v[2] = 210. * x[3];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[2];
	v[2] = v[0] - v[3];
	v[0] = 210. * x[4];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[0];
	v[0] = v[2] - v[3];
	v[2] = v[0] * v[0];
	v[1] += v[2];
	v[2] = 0.478 - x[0];
	v[0] = 220. * x[3];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[0];
	v[0] = v[2] - v[3];
	v[2] = 220. * x[4];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[2];
	v[2] = v[0] - v[3];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 0.467 - x[0];
	v[2] = 230. * x[3];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[2];
	v[2] = v[0] - v[3];
	v[0] = 230. * x[4];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[0];
	v[0] = v[2] - v[3];
	v[2] = v[0] * v[0];
	v[1] += v[2];
	v[2] = 0.457 - x[0];
	v[0] = 240. * x[3];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[0];
	v[0] = v[2] - v[3];
	v[2] = 240. * x[4];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[2];
	v[2] = v[0] - v[3];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 0.448 - x[0];
	v[2] = 250. * x[3];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[2];
	v[2] = v[0] - v[3];
	v[0] = 250. * x[4];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[0];
	v[0] = v[2] - v[3];
	v[2] = v[0] * v[0];
	v[1] += v[2];
	v[2] = 0.438 - x[0];
	v[0] = 260. * x[3];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[0];
	v[0] = v[2] - v[3];
	v[2] = 260. * x[4];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[2];
	v[2] = v[0] - v[3];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 0.431 - x[0];
	v[2] = 270. * x[3];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[2];
	v[2] = v[0] - v[3];
	v[0] = 270. * x[4];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[0];
	v[0] = v[2] - v[3];
	v[2] = v[0] * v[0];
	v[1] += v[2];
	v[2] = 0.424 - x[0];
	v[0] = 280. * x[3];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[0];
	v[0] = v[2] - v[3];
	v[2] = 280. * x[4];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[2];
	v[2] = v[0] - v[3];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 0.42 - x[0];
	v[2] = 290. * x[3];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[2];
	v[2] = v[0] - v[3];
	v[0] = 290. * x[4];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[0];
	v[0] = v[2] - v[3];
	v[2] = v[0] * v[0];
	v[1] += v[2];
	v[2] = 0.414 - x[0];
	v[0] = 300. * x[3];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[0];
	v[0] = v[2] - v[3];
	v[2] = 300. * x[4];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[2];
	v[2] = v[0] - v[3];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 0.411 - x[0];
	v[2] = 310. * x[3];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[2];
	v[2] = v[0] - v[3];
	v[0] = 310. * x[4];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[0];
	v[0] = v[2] - v[3];
	v[2] = v[0] * v[0];
	v[1] += v[2];
	v[2] = 0.406 - x[0];
	v[0] = 320. * x[3];
	v[3] = -v[0];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[1] * v[0];
	v[0] = v[2] - v[3];
	v[2] = 320. * x[4];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[2] * v[2];
	v[2] = v[0] - v[3];
	v[0] = v[2] * v[2];
	v[1] += v[0];

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
