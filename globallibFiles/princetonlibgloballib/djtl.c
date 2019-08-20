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
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80};

 real x0comn_[2] = {
		15.,
		-1. };

 real
feval0_(fint *nobj, real *x)
{
	real v[10];


  /***  objective ***/

	v[0] = -10. + x[0];
	v[1] = pow(v[0], 3.);
	if (errno) in_trouble2("pow",v[0],3.);
	v[0] = -20. + x[1];
	v[2] = pow(v[0], 3.);
	if (errno) in_trouble2("pow",v[0],3.);
	v[1] += v[2];
	v[2] = -5. + x[0];
	v[0] = v[2] * v[2];
	v[2] = -v[0];
	v[0] = -5. + x[1];
	v[3] = v[0] * v[0];
	v[0] = v[2] - v[3];
	v[2] = 201. + v[0];
	if (v[2] > 0.) goto L1;
	v[2] = -5. + x[0];
	v[0] = v[2] * v[2];
	v[2] = -v[0];
	v[0] = -5. + x[1];
	v[3] = v[0] * v[0];
	v[0] = v[2] - v[3];
	v[2] = 200. + v[0];
	v[0] = v[2] * v[2];
	v[2] = 1.e+10 * v[0];
	v[3] = v[2];
	goto L2;
 L1:
	v[2] = -5. + x[0];
	v[0] = v[2] * v[2];
	v[2] = -v[0];
	v[0] = -5. + x[1];
	v[3] = v[0] * v[0];
	v[0] = v[2] - v[3];
	v[2] = 201. + v[0];
	v[0] = log(v[2]);
	if (errno) in_trouble("log",v[2]);
	v[2] = -v[0];
	v[0] = -5. + x[0];
	v[3] = v[0] * v[0];
	v[0] = -5. + x[1];
	v[4] = v[0] * v[0];
	v[0] = v[3] + v[4];
	v[3] = -99. + v[0];
	if (v[3] > 0.) goto L3;
	v[3] = -5. + x[0];
	v[0] = v[3] * v[3];
	v[3] = -5. + x[1];
	v[4] = v[3] * v[3];
	v[3] = v[0] + v[4];
	v[0] = -100. + v[3];
	v[3] = v[0] * v[0];
	v[0] = 1.e+10 * v[3];
	v[5] = v[0];
	goto L4;
 L3:
	v[0] = -5. + x[0];
	v[3] = v[0] * v[0];
	v[0] = -5. + x[1];
	v[4] = v[0] * v[0];
	v[0] = v[3] + v[4];
	v[3] = -99. + v[0];
	v[0] = log(v[3]);
	if (errno) in_trouble("log",v[3]);
	v[3] = -v[0];
	v[0] = -5. + x[1];
	v[4] = v[0] * v[0];
	v[4] += 1.;
	v[0] = -6. + x[0];
	v[5] = v[0] * v[0];
	v[4] += v[5];
	if (v[4] > 0.) goto L5;
	v[4] = -5. + x[1];
	v[5] = v[4] * v[4];
	v[4] = -6. + x[0];
	v[0] = v[4] * v[4];
	v[4] = v[5] + v[0];
	v[5] = v[4] * v[4];
	v[4] = 1.e+10 * v[5];
	v[0] = v[4];
	goto L6;
 L5:
	v[4] = -5. + x[1];
	v[5] = v[4] * v[4];
	v[5] += 1.;
	v[4] = -6. + x[0];
	v[0] = v[4] * v[4];
	v[5] += v[0];
	v[0] = log(v[5]);
	if (errno) in_trouble("log",v[5]);
	v[5] = -v[0];
	v[0] = -5. + x[1];
	v[4] = v[0] * v[0];
	v[0] = -v[4];
	v[4] = -6. + x[0];
	v[6] = v[4] * v[4];
	v[4] = v[0] - v[6];
	v[0] = 83.81 + v[4];
	if (v[0] > 0.) goto L7;
	v[0] = -5. + x[1];
	v[4] = v[0] * v[0];
	v[0] = -v[4];
	v[4] = -6. + x[0];
	v[6] = v[4] * v[4];
	v[4] = v[0] - v[6];
	v[0] = 82.81 + v[4];
	v[4] = v[0] * v[0];
	v[0] = 1.e+10 * v[4];
	v[4] = v[0];
	goto L8;
 L7:
	v[0] = -5. + x[1];
	v[4] = v[0] * v[0];
	v[0] = -v[4];
	v[4] = -6. + x[0];
	v[6] = v[4] * v[4];
	v[4] = v[0] - v[6];
	v[0] = 83.81 + v[4];
	v[4] = log(v[0]);
	if (errno) in_trouble("log",v[0]);
	v[0] = -v[4];
	v[4] = -x[0];
	v[6] = 101. + v[4];
	if (v[6] > 0.) goto L9;
	v[6] = 100. - x[0];
	v[4] = v[6] * v[6];
	v[6] = 1.e+10 * v[4];
	goto L10;
 L9:
	v[6] = -x[0];
	v[4] = 101. + v[6];
	v[6] = log(v[4]);
	if (errno) in_trouble("log",v[4]);
	v[4] = -v[6];
	v[6] = -12. + x[0];
	if (v[6] > 0.) goto L11;
	v[6] = -13. + x[0];
	v[7] = v[6] * v[6];
	v[6] = 1.e+10 * v[7];
	v[7] = v[6];
	goto L12;
 L11:
	v[6] = -12. + x[0];
	v[7] = log(v[6]);
	if (errno) in_trouble("log",v[6]);
	v[6] = -v[7];
	v[7] = -x[1];
	v[8] = 101. + v[7];
	if (v[8] > 0.) goto L13;
	v[8] = 100. - x[1];
	v[7] = v[8] * v[8];
	v[8] = 1.e+10 * v[7];
	goto L14;
 L13:
	v[8] = -x[1];
	v[7] = 101. + v[8];
	v[8] = log(v[7]);
	if (errno) in_trouble("log",v[7]);
	v[7] = -v[8];
	v[8] = 1. + x[1];
	if (v[8] > 0.) goto L15;
	v[8] = x[1] * x[1];
	v[9] = 1.e+10 * v[8];
	goto L16;
 L15:
	v[9] = 1. + x[1];
	v[8] = log(v[9]);
	if (errno) in_trouble("log",v[9]);
	v[9] = -v[8];
 L16:
	v[8] = v[7] + v[9];
 L14:
	v[7] = v[6] + v[8];
 L12:
	v[6] = v[4] + v[7];
 L10:
	v[4] = v[0] + v[6];
 L8:
	v[0] = v[5] + v[4];
 L6:
	v[5] = v[3] + v[0];
 L4:
	v[3] = v[2] + v[5];
 L2:
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
