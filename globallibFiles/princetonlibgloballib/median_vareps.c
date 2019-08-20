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
		1.e-08,
		1.7e80,
		-1.7e80,
		1.7e80};

 real x0comn_[2] = {
		1.,
		0.5 };

 real
feval0_(fint *nobj, real *x)
{
	real v[4], rv;


  /***  objective ***/

	v[0] = x[0] * x[0];
	v[1] = -0.6092090562543065 + x[1];
	v[2] = v[1] * v[1];
	v[1] = v[0] + v[2];
	v[0] = sqrt(v[1]);
	if (errno) in_trouble("sqrt",v[1]);
	v[1] = x[0] * x[0];
	v[2] = -0.1898730530200304 + x[1];
	v[3] = v[2] * v[2];
	v[2] = v[1] + v[3];
	v[1] = sqrt(v[2]);
	if (errno) in_trouble("sqrt",v[2]);
	v[0] += v[1];
	v[1] = x[0] * x[0];
	v[2] = -0.9218920519490184 + x[1];
	v[3] = v[2] * v[2];
	v[2] = v[1] + v[3];
	v[1] = sqrt(v[2]);
	if (errno) in_trouble("sqrt",v[2]);
	v[0] += v[1];
	v[1] = x[0] * x[0];
	v[2] = -0.9571557013775424 + x[1];
	v[3] = v[2] * v[2];
	v[2] = v[1] + v[3];
	v[1] = sqrt(v[2]);
	if (errno) in_trouble("sqrt",v[2]);
	v[0] += v[1];
	v[1] = x[0] * x[0];
	v[2] = -0.10572581989835575 + x[1];
	v[3] = v[2] * v[2];
	v[2] = v[1] + v[3];
	v[1] = sqrt(v[2]);
	if (errno) in_trouble("sqrt",v[2]);
	v[0] += v[1];
	v[1] = x[0] * x[0];
	v[2] = -0.7141062486427147 + x[1];
	v[3] = v[2] * v[2];
	v[2] = v[1] + v[3];
	v[1] = sqrt(v[2]);
	if (errno) in_trouble("sqrt",v[2]);
	v[0] += v[1];
	v[1] = x[0] * x[0];
	v[2] = -0.5515320687097053 + x[1];
	v[3] = v[2] * v[2];
	v[2] = v[1] + v[3];
	v[1] = sqrt(v[2]);
	if (errno) in_trouble("sqrt",v[2]);
	v[0] += v[1];
	v[1] = x[0] * x[0];
	v[2] = -0.26313507221726584 + x[1];
	v[3] = v[2] * v[2];
	v[2] = v[1] + v[3];
	v[1] = sqrt(v[2]);
	if (errno) in_trouble("sqrt",v[2]);
	v[0] += v[1];
	v[1] = x[0] * x[0];
	v[2] = -0.3496041535777193 + x[1];
	v[3] = v[2] * v[2];
	v[2] = v[1] + v[3];
	v[1] = sqrt(v[2]);
	if (errno) in_trouble("sqrt",v[2]);
	v[0] += v[1];
	v[1] = x[0] * x[0];
	v[2] = -0.40724659389865475 + x[1];
	v[3] = v[2] * v[2];
	v[2] = v[1] + v[3];
	v[1] = sqrt(v[2]);
	if (errno) in_trouble("sqrt",v[2]);
	v[0] += v[1];
	v[1] = x[0] * x[0];
	v[2] = -0.6652117418059373 + x[1];
	v[3] = v[2] * v[2];
	v[2] = v[1] + v[3];
	v[1] = sqrt(v[2]);
	if (errno) in_trouble("sqrt",v[2]);
	v[0] += v[1];
	v[1] = x[0] * x[0];
	v[2] = -0.5758072295953904 + x[1];
	v[3] = v[2] * v[2];
	v[2] = v[1] + v[3];
	v[1] = sqrt(v[2]);
	if (errno) in_trouble("sqrt",v[2]);
	v[0] += v[1];
	v[1] = x[0] * x[0];
	v[2] = -0.9420215552039096 + x[1];
	v[3] = v[2] * v[2];
	v[2] = v[1] + v[3];
	v[1] = sqrt(v[2]);
	if (errno) in_trouble("sqrt",v[2]);
	v[0] += v[1];
	v[1] = x[0] * x[0];
	v[2] = -0.36352462623566634 + x[1];
	v[3] = v[2] * v[2];
	v[2] = v[1] + v[3];
	v[1] = sqrt(v[2]);
	if (errno) in_trouble("sqrt",v[2]);
	v[0] += v[1];
	v[1] = x[0] * x[0];
	v[2] = -0.003088755058514279 + x[1];
	v[3] = v[2] * v[2];
	v[2] = v[1] + v[3];
	v[1] = sqrt(v[2]);
	if (errno) in_trouble("sqrt",v[2]);
	v[0] += v[1];
	v[1] = x[0] * x[0];
	v[2] = -0.7555983360222088 + x[1];
	v[3] = v[2] * v[2];
	v[2] = v[1] + v[3];
	v[1] = sqrt(v[2]);
	if (errno) in_trouble("sqrt",v[2]);
	v[0] += v[1];
	v[1] = x[0] * x[0];
	v[2] = -0.4501026215672097 + x[1];
	v[3] = v[2] * v[2];
	v[2] = v[1] + v[3];
	v[1] = sqrt(v[2]);
	if (errno) in_trouble("sqrt",v[2]);
	v[0] += v[1];
	v[1] = x[0] * x[0];
	v[2] = -0.17012187265331485 + x[1];
	v[3] = v[2] * v[2];
	v[2] = v[1] + v[3];
	v[1] = sqrt(v[2]);
	if (errno) in_trouble("sqrt",v[2]);
	v[0] += v[1];
	v[1] = x[0] * x[0];
	v[2] = -0.7877480635271975 + x[1];
	v[3] = v[2] * v[2];
	v[2] = v[1] + v[3];
	v[1] = sqrt(v[2]);
	if (errno) in_trouble("sqrt",v[2]);
	v[0] += v[1];
	rv = v[0] + x[0];

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
