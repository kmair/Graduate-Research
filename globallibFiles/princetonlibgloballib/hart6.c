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
	6 /* nvar */,
	1 /* nobj */,
	0 /* ncon */,
	0 /* nzc */,
	0 /* densejac */,

	/* objtype (0 = minimize, 1 = maximize) */

	0 };

 real boundc_[1+12+0] /* Infinity, variable bounds, constraint bounds */ = {
		1.7e80,
		0.,
		1.,
		0.,
		1.,
		0.,
		1.,
		0.,
		1.,
		0.,
		1.,
		0.,
		1.};

 real x0comn_[6] = {
		0.2,
		0.2,
		0.2,
		0.2,
		0.2,
		0.2 };

 real
feval0_(fint *nobj, real *x)
{
	real v[4];


  /***  objective ***/

	v[0] = -0.1312 + x[0];
	v[1] = v[0] * v[0];
	v[0] = 10. * v[1];
	v[1] = -0.1696 + x[1];
	v[2] = v[1] * v[1];
	v[1] = 0.05 * v[2];
	v[0] += v[1];
	v[1] = -0.5569 + x[2];
	v[2] = v[1] * v[1];
	v[1] = 17. * v[2];
	v[0] += v[1];
	v[1] = -0.0124 + x[3];
	v[2] = v[1] * v[1];
	v[1] = 3.5 * v[2];
	v[0] += v[1];
	v[1] = -0.8283 + x[4];
	v[2] = v[1] * v[1];
	v[1] = 1.7 * v[2];
	v[0] += v[1];
	v[1] = -0.5886 + x[5];
	v[2] = v[1] * v[1];
	v[1] = 8. * v[2];
	v[0] += v[1];
	v[1] = -v[0];
	v[0] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = -v[0];
	v[0] = -0.2329 + x[0];
	v[2] = v[0] * v[0];
	v[0] = 0.05 * v[2];
	v[2] = -0.4135 + x[1];
	v[3] = v[2] * v[2];
	v[2] = 10. * v[3];
	v[0] += v[2];
	v[2] = -0.8307 + x[2];
	v[3] = v[2] * v[2];
	v[2] = 17. * v[3];
	v[0] += v[2];
	v[2] = -0.3736 + x[3];
	v[3] = v[2] * v[2];
	v[2] = 0.1 * v[3];
	v[0] += v[2];
	v[2] = -0.1004 + x[4];
	v[3] = v[2] * v[2];
	v[2] = 8. * v[3];
	v[0] += v[2];
	v[2] = -0.9991 + x[5];
	v[3] = v[2] * v[2];
	v[2] = 14. * v[3];
	v[0] += v[2];
	v[2] = -v[0];
	v[0] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = -1.2 * v[0];
	v[1] += v[2];
	v[2] = -0.2348 + x[0];
	v[0] = v[2] * v[2];
	v[2] = 3. * v[0];
	v[0] = -0.1451 + x[1];
	v[3] = v[0] * v[0];
	v[0] = 3.5 * v[3];
	v[2] += v[0];
	v[0] = -0.3522 + x[2];
	v[3] = v[0] * v[0];
	v[0] = 1.7 * v[3];
	v[2] += v[0];
	v[0] = -0.2883 + x[3];
	v[3] = v[0] * v[0];
	v[0] = 10. * v[3];
	v[2] += v[0];
	v[0] = -0.3047 + x[4];
	v[3] = v[0] * v[0];
	v[0] = 17. * v[3];
	v[2] += v[0];
	v[0] = -0.665 + x[5];
	v[3] = v[0] * v[0];
	v[0] = 8. * v[3];
	v[2] += v[0];
	v[0] = -v[2];
	v[2] = exp(v[0]);
	if (errno) in_trouble("exp",v[0]);
	v[0] = -3. * v[2];
	v[1] += v[0];
	v[0] = -0.4047 + x[0];
	v[2] = v[0] * v[0];
	v[0] = 17. * v[2];
	v[2] = -0.8828 + x[1];
	v[3] = v[2] * v[2];
	v[2] = 8. * v[3];
	v[0] += v[2];
	v[2] = -0.8732 + x[2];
	v[3] = v[2] * v[2];
	v[2] = 0.05 * v[3];
	v[0] += v[2];
	v[2] = -0.5743 + x[3];
	v[3] = v[2] * v[2];
	v[2] = 10. * v[3];
	v[0] += v[2];
	v[2] = -0.1091 + x[4];
	v[3] = v[2] * v[2];
	v[2] = 0.1 * v[3];
	v[0] += v[2];
	v[2] = -0.0381 + x[5];
	v[3] = v[2] * v[2];
	v[2] = 14. * v[3];
	v[0] += v[2];
	v[2] = -v[0];
	v[0] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = -3.2 * v[0];
	v[1] += v[2];

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

x_input = malloc (6 * sizeof(real));
input_values = malloc (6 * sizeof(double));
c_val = malloc (0 * sizeof(real));

file_input = fopen("input.in","r");
for (i=0; i < 6; i++)
    fscanf(file_input, "%lf" ,&input_values[i]);

fclose(file_input);
for (i=0; i < 6; i++)
 {
    x_input[i] = input_values[i];
 }

f_val = feval0_(&objective_number, x_input);

FILE *output_out;
output_out = fopen ("output.out","w");
fprintf(output_out,"%30.15f\n",f_val);
fclose(output_out);

}
