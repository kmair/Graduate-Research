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
	2 /* nvar */,
	1 /* nobj */,
	0 /* ncon */,
	0 /* nzc */,
	0 /* densejac */,

	/* objtype (0 = minimize, 1 = maximize) */

	0 };

 real boundc_[1+4+0] /* Infinity, variable bounds, constraint bounds */ = {
		1.7e80,
		0.,
		95.,
		0.,
		75.};

 real x0comn_[2] = {
		95.,
		10. };

 real
feval0_(fint *nobj, real *x)
{
	real v[4], rv;


  /***  objective ***/

	v[0] = x[0] * x[0];
	v[1] = -0.1269366345 * v[0];
	v[0] = pow(x[0], 3.);
	if (errno) in_trouble2("pow",x[0],3.);
	v[2] = 0.0020567665 * v[0];
	v[1] += v[2];
	v[2] = pow(x[0], 4.);
	if (errno) in_trouble2("pow",x[0],4.);
	v[0] = -1.0345e-05 * v[2];
	v[1] += v[0];
	v[0] = 0.0302344793 * x[0];
	v[2] = x[0] * x[0];
	v[3] = -0.0012813448 * v[2];
	v[0] += v[3];
	v[3] = pow(x[0], 3.);
	if (errno) in_trouble2("pow",x[0],3.);
	v[2] = 3.52599e-05 * v[3];
	v[0] += v[2];
	v[2] = pow(x[0], 4.);
	if (errno) in_trouble2("pow",x[0],4.);
	v[3] = -2.266e-07 * v[2];
	v[0] += v[3];
	v[3] = x[1] * v[0];
	v[0] = -v[3];
	v[1] += v[0];
	v[0] = x[1] * x[1];
	v[3] = -0.2564581253 * v[0];
	v[1] += v[3];
	v[3] = pow(x[1], 3.);
	if (errno) in_trouble2("pow",x[1],3.);
	v[0] = 0.003460403 * v[3];
	v[1] += v[0];
	v[0] = pow(x[1], 4.);
	if (errno) in_trouble2("pow",x[1],4.);
	v[3] = -1.35139e-05 * v[0];
	v[1] += v[3];
	v[3] = 1. + x[1];
	if (v[3] == 0.) {
	zerdiv_(&v[3]);	}
	v[0] = -28.1064434908 / v[3];
	v[3] = -v[0];
	v[1] += v[3];
	v[3] = 0.0003405462 * x[0];
	v[0] = x[0] * x[0];
	v[2] = -5.2375e-06 * v[0];
	v[3] += v[2];
	v[2] = pow(x[0], 3.);
	if (errno) in_trouble2("pow",x[0],3.);
	v[0] = -6.3e-09 * v[2];
	v[3] += v[0];
	v[0] = x[1] * x[1];
	v[2] = v[3] * v[0];
	v[3] = -v[2];
	v[1] += v[3];
	v[3] = pow(x[0], 3.);
	if (errno) in_trouble2("pow",x[0],3.);
	v[2] = 7.e-10 * v[3];
	v[3] = -1.6638e-06 * x[0];
	v[0] = v[2] + v[3];
	v[2] = pow(x[1], 3.);
	if (errno) in_trouble2("pow",x[1],3.);
	v[3] = v[0] * v[2];
	v[0] = -v[3];
	v[1] += v[0];
	v[0] = 0.0005 * x[0];
	v[3] = v[0] * x[1];
	v[0] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = 2.8673112392 * v[0];
	v[1] += v[3];
	v[1] += -75.1963666677;
	rv = v[1] + 3.8112755343*x[0];
	rv += 6.8306567613*x[1];

	return rv;
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

x_input = malloc (2 * sizeof(real));
input_values = malloc (2 * sizeof(double));
c_val = malloc (0 * sizeof(real));

file_input = fopen("input.in","r");
for (i=0; i < 2; i++)
    fscanf(file_input, "%lf" ,&input_values[i]);

fclose(file_input);
for (i=0; i < 2; i++)
 {
    x_input[i] = input_values[i];
 }

f_val = feval0_(&objective_number, x_input);

FILE *output_out;
output_out = fopen ("output.out","w");
fprintf(output_out,"%30.15f\n",f_val);
fclose(output_out);

}
