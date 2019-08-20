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
	9 /* nvar */,
	1 /* nobj */,
	0 /* ncon */,
	0 /* nzc */,
	0 /* densejac */,

	/* objtype (0 = minimize, 1 = maximize) */

	0 };

 real boundc_[1+18+0] /* Infinity, variable bounds, constraint bounds */ = {
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

 real x0comn_[9] = {
		0.16829419696157932,
		-0.15136049906158566,
		0.08242369704835133,
		-0.057580663333013066,
		-0.026470350019554608,
		-0.19835577068862317,
		0.7630021222075776,
		0.18400520763935813,
		-0.12597759885489077 };

 real
feval0_(fint *nobj, real *x)
{
	real v[3];


  /***  objective ***/

	v[0] = x[0] * x[0];
	v[1] = x[1] * x[0];
	v[0] += v[1];
	v[1] = x[2] * x[0];
	v[0] += v[1];
	v[1] = -0.9259593667331212 + v[0];
	v[0] = v[1] * v[1];
	v[1] = x[0] * x[1];
	v[2] = x[1] * x[1];
	v[1] += v[2];
	v[2] = x[2] * x[1];
	v[1] += v[2];
	v[2] = 0.15750346905483398 + v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = x[0] * x[2];
	v[2] = x[1] * x[2];
	v[1] += v[2];
	v[2] = x[2] * x[2];
	v[1] += v[2];
	v[2] = -0.8377779726409494 + v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = x[3] * x[3];
	v[2] = x[4] * x[3];
	v[1] += v[2];
	v[2] = x[5] * x[3];
	v[1] += v[2];
	v[2] = 0.20415777958403747 + v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = x[3] * x[4];
	v[2] = x[4] * x[4];
	v[1] += v[2];
	v[2] = x[5] * x[4];
	v[1] += v[2];
	v[2] = 0.6770594350871318 + v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = x[3] * x[5];
	v[2] = x[4] * x[5];
	v[1] += v[2];
	v[2] = x[5] * x[5];
	v[1] += v[2];
	v[2] = -0.6373229809621763 + v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = x[6] * x[6];
	v[2] = x[7] * x[6];
	v[1] += v[2];
	v[2] = x[8] * x[6];
	v[1] += v[2];
	v[2] = 0.2648785478150761 + v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = x[6] * x[7];
	v[2] = x[7] * x[7];
	v[1] += v[2];
	v[2] = x[8] * x[7];
	v[1] += v[2];
	v[2] = 0.7012804121709143 + v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = x[6] * x[8];
	v[2] = x[7] * x[8];
	v[1] += v[2];
	v[2] = x[8] * x[8];
	v[1] += v[2];
	v[2] = 0.5157034839695309 + v[1];
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

x_input = malloc (9 * sizeof(real));
input_values = malloc (9 * sizeof(double));
c_val = malloc (0 * sizeof(real));

file_input = fopen("input.in","r");
for (i=0; i < 9; i++)
    fscanf(file_input, "%lf" ,&input_values[i]);

fclose(file_input);
for (i=0; i < 9; i++)
 {
    x_input[i] = input_values[i];
 }

f_val = feval0_(&objective_number, x_input);

FILE *output_out;
output_out = fopen ("output.out","w");
fprintf(output_out,"%30.15f\n",f_val);
fclose(output_out);

}
