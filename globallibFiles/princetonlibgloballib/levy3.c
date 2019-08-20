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
		-10.,
		10.,
		-10.,
		10.};

 real x0comn_[2] = {
		4.976,
		4.976 };

 real
feval0_(fint *nobj, real *x)
{
	real v[4];


  /***  objective ***/

	v[0] = 2. * x[0];
	v[1] = 1. + v[0];
	v[0] = cos(v[1]);
	if (errno) in_trouble("cos",v[1]);
	v[1] = 3. * x[0];
	v[2] = 2. + v[1];
	v[1] = cos(v[2]);
	if (errno) in_trouble("cos",v[2]);
	v[2] = 2. * v[1];
	v[0] += v[2];
	v[2] = 4. * x[0];
	v[1] = 3. + v[2];
	v[2] = cos(v[1]);
	if (errno) in_trouble("cos",v[1]);
	v[1] = 3. * v[2];
	v[0] += v[1];
	v[1] = 5. * x[0];
	v[2] = 4. + v[1];
	v[1] = cos(v[2]);
	if (errno) in_trouble("cos",v[2]);
	v[2] = 4. * v[1];
	v[0] += v[2];
	v[2] = 6. * x[0];
	v[1] = 5. + v[2];
	v[2] = cos(v[1]);
	if (errno) in_trouble("cos",v[1]);
	v[1] = 5. * v[2];
	v[0] += v[1];
	v[1] = 2. * x[1];
	v[2] = 1. + v[1];
	v[1] = cos(v[2]);
	if (errno) in_trouble("cos",v[2]);
	v[2] = 3. * x[1];
	v[3] = 2. + v[2];
	v[2] = cos(v[3]);
	if (errno) in_trouble("cos",v[3]);
	v[3] = 2. * v[2];
	v[1] += v[3];
	v[3] = 4. * x[1];
	v[2] = 3. + v[3];
	v[3] = cos(v[2]);
	if (errno) in_trouble("cos",v[2]);
	v[2] = 3. * v[3];
	v[1] += v[2];
	v[2] = 5. * x[1];
	v[3] = 4. + v[2];
	v[2] = cos(v[3]);
	if (errno) in_trouble("cos",v[3]);
	v[3] = 4. * v[2];
	v[1] += v[3];
	v[3] = 6. * x[1];
	v[2] = 5. + v[3];
	v[3] = cos(v[2]);
	if (errno) in_trouble("cos",v[2]);
	v[2] = 5. * v[3];
	v[1] += v[2];
	v[2] = v[0] * v[1];
	v[0] = -v[2];

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