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
	17 /* nvar */,
	1 /* nobj */,
	0 /* ncon */,
	0 /* nzc */,
	0 /* densejac */,

	/* objtype (0 = minimize, 1 = maximize) */

	0 };

 real boundc_[1+34+0] /* Infinity, variable bounds, constraint bounds */ = {
		1.7e80,
		1.e-08,
		1.7e80,
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80};

 real x0comn_[17] = {
		1.,
		0.6092090562543065,
		0.1898730530200304,
		0.9218920519490184,
		0.9571557013775424,
		0.10572581989835575,
		0.7141062486427147,
		0.5515320687097053,
		0.26313507221726584,
		0.3496041535777193,
		0.40724659389865475,
		0.6652117418059373,
		0.5758072295953904,
		0.9420215552039096,
		0.36352462623566634,
		0.003088755058514279,
		0.7555983360222088 };

 real
feval0_(fint *nobj, real *x)
{
	real v[5], rv;


  /***  objective ***/

	v[0] = x[0] * x[0];
	v[1] = 2.309469 - x[13];
	v[2] = v[1] * v[1];
	v[1] = 9.208211 - x[14];
	v[3] = v[1] * v[1];
	v[1] = v[2] + v[3];
	v[2] = v[0] + v[1];
	v[0] = sqrt(v[2]);
	if (errno) in_trouble("sqrt",v[2]);
	v[2] = x[0] * x[0];
	v[1] = 0.577367 - x[1];
	v[3] = v[1] * v[1];
	v[1] = 6.480938 - x[2];
	v[4] = v[1] * v[1];
	v[1] = v[3] + v[4];
	v[3] = v[2] + v[1];
	v[2] = sqrt(v[3]);
	if (errno) in_trouble("sqrt",v[3]);
	v[0] += v[2];
	v[2] = x[0] * x[0];
	v[3] = 0.808314 - x[3];
	v[1] = v[3] * v[3];
	v[3] = 3.519062 - x[4];
	v[4] = v[3] * v[3];
	v[3] = v[1] + v[4];
	v[1] = v[2] + v[3];
	v[2] = sqrt(v[1]);
	if (errno) in_trouble("sqrt",v[1]);
	v[0] += v[2];
	v[2] = x[0] * x[0];
	v[1] = 1.685912 - x[5];
	v[3] = v[1] * v[1];
	v[1] = 1.231672 - x[6];
	v[4] = v[1] * v[1];
	v[1] = v[3] + v[4];
	v[3] = v[2] + v[1];
	v[2] = sqrt(v[3]);
	if (errno) in_trouble("sqrt",v[3]);
	v[0] += v[2];
	v[2] = x[0] * x[0];
	v[3] = 4.110855 - x[7];
	v[1] = v[3] * v[3];
	v[3] = 0.821114 - x[8];
	v[4] = v[3] * v[3];
	v[3] = v[1] + v[4];
	v[1] = v[2] + v[3];
	v[2] = sqrt(v[1]);
	if (errno) in_trouble("sqrt",v[1]);
	v[0] += v[2];
	v[2] = x[0] * x[0];
	v[1] = 7.598152 - x[9];
	v[3] = v[1] * v[1];
	v[1] = 0.615836 - x[10];
	v[4] = v[1] * v[1];
	v[1] = v[3] + v[4];
	v[3] = v[2] + v[1];
	v[2] = sqrt(v[3]);
	if (errno) in_trouble("sqrt",v[3]);
	v[0] += v[2];
	v[2] = x[0] * x[0];
	v[3] = 8.568129 - x[9];
	v[1] = v[3] * v[3];
	v[3] = 3.079179 - x[10];
	v[4] = v[3] * v[3];
	v[3] = v[1] + v[4];
	v[1] = v[2] + v[3];
	v[2] = sqrt(v[1]);
	if (errno) in_trouble("sqrt",v[1]);
	v[0] += v[2];
	v[2] = x[0] * x[0];
	v[1] = 4.757506 - x[11];
	v[3] = v[1] * v[1];
	v[1] = 3.753666 - x[12];
	v[4] = v[1] * v[1];
	v[1] = v[3] + v[4];
	v[3] = v[2] + v[1];
	v[2] = sqrt(v[3]);
	if (errno) in_trouble("sqrt",v[3]);
	v[0] += v[2];
	v[2] = x[0] * x[0];
	v[3] = 3.926097 - x[15];
	v[1] = v[3] * v[3];
	v[3] = 7.008798 - x[16];
	v[4] = v[3] * v[3];
	v[3] = v[1] + v[4];
	v[1] = v[2] + v[3];
	v[2] = sqrt(v[1]);
	if (errno) in_trouble("sqrt",v[1]);
	v[0] += v[2];
	v[2] = x[0] * x[0];
	v[1] = 7.43649 - x[15];
	v[3] = v[1] * v[1];
	v[1] = 7.683284 - x[16];
	v[4] = v[1] * v[1];
	v[1] = v[3] + v[4];
	v[3] = v[2] + v[1];
	v[2] = sqrt(v[3]);
	if (errno) in_trouble("sqrt",v[3]);
	v[0] += v[2];
	v[2] = x[0] * x[0];
	v[3] = x[9] - x[11];
	v[1] = v[3] * v[3];
	v[3] = x[10] - x[12];
	v[4] = v[3] * v[3];
	v[3] = v[1] + v[4];
	v[1] = v[2] + v[3];
	v[2] = sqrt(v[1]);
	if (errno) in_trouble("sqrt",v[1]);
	v[0] += v[2];
	v[2] = x[0] * x[0];
	v[1] = x[11] - x[7];
	v[3] = v[1] * v[1];
	v[1] = x[12] - x[8];
	v[4] = v[1] * v[1];
	v[1] = v[3] + v[4];
	v[3] = v[2] + v[1];
	v[2] = sqrt(v[3]);
	if (errno) in_trouble("sqrt",v[3]);
	v[0] += v[2];
	v[2] = x[0] * x[0];
	v[3] = x[7] - x[5];
	v[1] = v[3] * v[3];
	v[3] = x[8] - x[6];
	v[4] = v[3] * v[3];
	v[3] = v[1] + v[4];
	v[1] = v[2] + v[3];
	v[2] = sqrt(v[1]);
	if (errno) in_trouble("sqrt",v[1]);
	v[0] += v[2];
	v[2] = x[0] * x[0];
	v[1] = x[5] - x[3];
	v[3] = v[1] * v[1];
	v[1] = x[6] - x[4];
	v[4] = v[1] * v[1];
	v[1] = v[3] + v[4];
	v[3] = v[2] + v[1];
	v[2] = sqrt(v[3]);
	if (errno) in_trouble("sqrt",v[3]);
	v[0] += v[2];
	v[2] = x[0] * x[0];
	v[3] = x[3] - x[1];
	v[1] = v[3] * v[3];
	v[3] = x[4] - x[2];
	v[4] = v[3] * v[3];
	v[3] = v[1] + v[4];
	v[1] = v[2] + v[3];
	v[2] = sqrt(v[1]);
	if (errno) in_trouble("sqrt",v[1]);
	v[0] += v[2];
	v[2] = x[0] * x[0];
	v[1] = x[1] - x[13];
	v[3] = v[1] * v[1];
	v[1] = x[2] - x[14];
	v[4] = v[1] * v[1];
	v[1] = v[3] + v[4];
	v[3] = v[2] + v[1];
	v[2] = sqrt(v[3]);
	if (errno) in_trouble("sqrt",v[3]);
	v[0] += v[2];
	v[2] = x[0] * x[0];
	v[3] = x[13] - x[15];
	v[1] = v[3] * v[3];
	v[3] = x[14] - x[16];
	v[4] = v[3] * v[3];
	v[3] = v[1] + v[4];
	v[1] = v[2] + v[3];
	v[2] = sqrt(v[1]);
	if (errno) in_trouble("sqrt",v[1]);
	v[0] += v[2];
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

x_input = malloc (17 * sizeof(real));
input_values = malloc (17 * sizeof(double));
c_val = malloc (0 * sizeof(real));

file_input = fopen("input.in","r");
for (i=0; i < 17; i++)
    fscanf(file_input, "%lf" ,&input_values[i]);

fclose(file_input);
for (i=0; i < 17; i++)
 {
    x_input[i] = input_values[i];
 }

f_val = feval0_(&objective_number, x_input);

FILE *output_out;
output_out = fopen ("output.out","w");
fprintf(output_out,"%30.15f\n",f_val);
fclose(output_out);

}
