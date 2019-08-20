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
	4 /* nvar */,
	1 /* nobj */,
	0 /* ncon */,
	0 /* nzc */,
	0 /* densejac */,

	/* objtype (0 = minimize, 1 = maximize) */

	0 };

 real boundc_[1+8+0] /* Infinity, variable bounds, constraint bounds */ = {
		1.7e80,
		-1.7e80,
		1.7e80,
		0.,
		1.7e80,
		1.e-06,
		1.7e80,
		1.e-06,
		1.7e80};

 real x0comn_[4] = {
		1.,
		1.,
		1.,
		1. };

 real
feval0_(fint *nobj, real *x)
{
	real v[4];


  /***  objective ***/

	v[0] = 2.749172911969 * x[0];
	if (x[3] == 0.) {
	zerdiv_(&x[3]);	}
	v[1] = 2.749172911969 / x[3];
	v[2] = x[2] + v[1];
	if (v[2] == 0.) {
	zerdiv_(&v[2]);	}
	v[1] = x[1] / v[2];
	v[2] = v[0] + v[1];
	v[0] = 64.87939 - v[2];
	v[2] = v[0] * v[0];
	v[0] = 2.4674000736160004 * x[0];
	if (x[3] == 0.) {
	zerdiv_(&x[3]);	}
	v[1] = 2.4674000736160004 / x[3];
	v[3] = x[2] + v[1];
	if (v[3] == 0.) {
	zerdiv_(&v[3]);	}
	v[1] = x[1] / v[3];
	v[3] = v[0] + v[1];
	v[0] = 50.46046 - v[3];
	v[3] = v[0] * v[0];
	v[2] += v[3];
	v[3] = 1.949550365169 * x[0];
	if (x[3] == 0.) {
	zerdiv_(&x[3]);	}
	v[0] = 1.949550365169 / x[3];
	v[1] = x[2] + v[0];
	if (v[1] == 0.) {
	zerdiv_(&v[1]);	}
	v[0] = x[1] / v[1];
	v[1] = v[3] + v[0];
	v[3] = 28.2034 - v[1];
	v[1] = v[3] * v[3];
	v[2] += v[1];
	v[1] = 1.4926241929 * x[0];
	if (x[3] == 0.) {
	zerdiv_(&x[3]);	}
	v[3] = 1.4926241929 / x[3];
	v[0] = x[2] + v[3];
	if (v[0] == 0.) {
	zerdiv_(&v[0]);	}
	v[3] = x[1] / v[0];
	v[0] = v[1] + v[3];
	v[1] = 13.4575 - v[0];
	v[0] = v[1] * v[1];
	v[2] += v[0];
	v[0] = 1.0966236512040002 * x[0];
	if (x[3] == 0.) {
	zerdiv_(&x[3]);	}
	v[1] = 1.0966236512040002 / x[3];
	v[3] = x[2] + v[1];
	if (v[3] == 0.) {
	zerdiv_(&v[3]);	}
	v[1] = x[1] / v[3];
	v[3] = v[0] + v[1];
	v[0] = 4.6547 - v[3];
	v[3] = v[0] * v[0];
	v[2] += v[3];
	v[3] = 0.7615442022250001 * x[0];
	if (x[3] == 0.) {
	zerdiv_(&x[3]);	}
	v[0] = 0.7615442022250001 / x[3];
	v[1] = x[2] + v[0];
	if (v[1] == 0.) {
	zerdiv_(&v[1]);	}
	v[0] = x[1] / v[1];
	v[1] = v[3] + v[0];
	v[3] = 0.59447 - v[1];
	v[1] = v[3] * v[3];
	v[2] += v[1];
	v[1] = 0.587569773961 * x[0];
	if (x[3] == 0.) {
	zerdiv_(&x[3]);	}
	v[3] = 0.587569773961 / x[3];
	v[0] = x[2] + v[3];
	if (v[0] == 0.) {
	zerdiv_(&v[0]);	}
	v[3] = x[1] / v[0];
	v[0] = v[1] + v[3];
	v[1] = -v[0];
	v[0] = v[1] * v[1];
	v[2] += v[0];
	v[0] = 0.487388289424 * x[0];
	if (x[3] == 0.) {
	zerdiv_(&x[3]);	}
	v[1] = 0.487388289424 / x[3];
	v[3] = x[2] + v[1];
	if (v[3] == 0.) {
	zerdiv_(&v[3]);	}
	v[1] = x[1] / v[3];
	v[3] = v[0] + v[1];
	v[0] = 0.2177 - v[3];
	v[3] = v[0] * v[0];
	v[2] += v[3];
	v[3] = 0.27415591280100005 * x[0];
	if (x[3] == 0.) {
	zerdiv_(&x[3]);	}
	v[0] = 0.27415591280100005 / x[3];
	v[1] = x[2] + v[0];
	if (v[1] == 0.) {
	zerdiv_(&v[1]);	}
	v[0] = x[1] / v[1];
	v[1] = v[3] + v[0];
	v[3] = 2.3029 - v[1];
	v[1] = v[3] * v[3];
	v[2] += v[1];
	v[1] = 0.121847072356 * x[0];
	if (x[3] == 0.) {
	zerdiv_(&x[3]);	}
	v[3] = 0.121847072356 / x[3];
	v[0] = x[2] + v[3];
	if (v[0] == 0.) {
	zerdiv_(&v[0]);	}
	v[3] = x[1] / v[0];
	v[0] = v[1] + v[3];
	v[1] = 5.5191 - v[0];
	v[0] = v[1] * v[1];
	v[2] += v[0];
	v[0] = 0.030461768089 * x[0];
	if (x[3] == 0.) {
	zerdiv_(&x[3]);	}
	v[1] = 0.030461768089 / x[3];
	v[3] = x[2] + v[1];
	if (v[3] == 0.) {
	zerdiv_(&v[3]);	}
	v[1] = x[1] / v[3];
	v[3] = v[0] + v[1];
	v[0] = 8.5519 - v[3];
	v[3] = v[0] * v[0];
	v[2] += v[3];
	if (x[2] == 0.) {
	zerdiv_(&x[2]);	}
	v[3] = x[1] / x[2];
	v[0] = 9.8919 - v[3];
	v[3] = v[0] * v[0];
	v[2] += v[3];
	v[3] = 0.030461768089 * x[0];
	if (x[3] == 0.) {
	zerdiv_(&x[3]);	}
	v[0] = 0.030461768089 / x[3];
	v[1] = x[2] + v[0];
	if (v[1] == 0.) {
	zerdiv_(&v[1]);	}
	v[0] = x[1] / v[1];
	v[1] = v[3] + v[0];
	v[3] = 8.5519 - v[1];
	v[1] = v[3] * v[3];
	v[2] += v[1];
	v[1] = 0.121847072356 * x[0];
	if (x[3] == 0.) {
	zerdiv_(&x[3]);	}
	v[3] = 0.121847072356 / x[3];
	v[0] = x[2] + v[3];
	if (v[0] == 0.) {
	zerdiv_(&v[0]);	}
	v[3] = x[1] / v[0];
	v[0] = v[1] + v[3];
	v[1] = 5.5191 - v[0];
	v[0] = v[1] * v[1];
	v[2] += v[0];
	v[0] = 0.27415591280100005 * x[0];
	if (x[3] == 0.) {
	zerdiv_(&x[3]);	}
	v[1] = 0.27415591280100005 / x[3];
	v[3] = x[2] + v[1];
	if (v[3] == 0.) {
	zerdiv_(&v[3]);	}
	v[1] = x[1] / v[3];
	v[3] = v[0] + v[1];
	v[0] = 2.3029 - v[3];
	v[3] = v[0] * v[0];
	v[2] += v[3];
	v[3] = 0.487388289424 * x[0];
	if (x[3] == 0.) {
	zerdiv_(&x[3]);	}
	v[0] = 0.487388289424 / x[3];
	v[1] = x[2] + v[0];
	if (v[1] == 0.) {
	zerdiv_(&v[1]);	}
	v[0] = x[1] / v[1];
	v[1] = v[3] + v[0];
	v[3] = 0.2177 - v[1];
	v[1] = v[3] * v[3];
	v[2] += v[1];
	v[1] = 0.587569773961 * x[0];
	if (x[3] == 0.) {
	zerdiv_(&x[3]);	}
	v[3] = 0.587569773961 / x[3];
	v[0] = x[2] + v[3];
	if (v[0] == 0.) {
	zerdiv_(&v[0]);	}
	v[3] = x[1] / v[0];
	v[0] = v[1] + v[3];
	v[1] = -v[0];
	v[0] = v[1] * v[1];
	v[2] += v[0];
	v[0] = 0.7615442022250001 * x[0];
	if (x[3] == 0.) {
	zerdiv_(&x[3]);	}
	v[1] = 0.7615442022250001 / x[3];
	v[3] = x[2] + v[1];
	if (v[3] == 0.) {
	zerdiv_(&v[3]);	}
	v[1] = x[1] / v[3];
	v[3] = v[0] + v[1];
	v[0] = 0.59447 - v[3];
	v[3] = v[0] * v[0];
	v[2] += v[3];
	v[3] = 1.0966236512040002 * x[0];
	if (x[3] == 0.) {
	zerdiv_(&x[3]);	}
	v[0] = 1.0966236512040002 / x[3];
	v[1] = x[2] + v[0];
	if (v[1] == 0.) {
	zerdiv_(&v[1]);	}
	v[0] = x[1] / v[1];
	v[1] = v[3] + v[0];
	v[3] = 4.6547 - v[1];
	v[1] = v[3] * v[3];
	v[2] += v[1];
	v[1] = 1.4926241929 * x[0];
	if (x[3] == 0.) {
	zerdiv_(&x[3]);	}
	v[3] = 1.4926241929 / x[3];
	v[0] = x[2] + v[3];
	if (v[0] == 0.) {
	zerdiv_(&v[0]);	}
	v[3] = x[1] / v[0];
	v[0] = v[1] + v[3];
	v[1] = 13.4575 - v[0];
	v[0] = v[1] * v[1];
	v[2] += v[0];
	v[0] = 1.949550365169 * x[0];
	if (x[3] == 0.) {
	zerdiv_(&x[3]);	}
	v[1] = 1.949550365169 / x[3];
	v[3] = x[2] + v[1];
	if (v[3] == 0.) {
	zerdiv_(&v[3]);	}
	v[1] = x[1] / v[3];
	v[3] = v[0] + v[1];
	v[0] = 28.2034 - v[3];
	v[3] = v[0] * v[0];
	v[2] += v[3];
	v[3] = 2.4674000736160004 * x[0];
	if (x[3] == 0.) {
	zerdiv_(&x[3]);	}
	v[0] = 2.4674000736160004 / x[3];
	v[1] = x[2] + v[0];
	if (v[1] == 0.) {
	zerdiv_(&v[1]);	}
	v[0] = x[1] / v[1];
	v[1] = v[3] + v[0];
	v[3] = 50.46046 - v[1];
	v[1] = v[3] * v[3];
	v[2] += v[1];
	v[1] = 2.749172911969 * x[0];
	if (x[3] == 0.) {
	zerdiv_(&x[3]);	}
	v[3] = 2.749172911969 / x[3];
	v[0] = x[2] + v[3];
	if (v[0] == 0.) {
	zerdiv_(&v[0]);	}
	v[3] = x[1] / v[0];
	v[0] = v[1] + v[3];
	v[1] = 64.87939 - v[0];
	v[0] = v[1] * v[1];
	v[2] += v[0];

	return v[2];
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

x_input = malloc (4 * sizeof(real));
input_values = malloc (4 * sizeof(double));
c_val = malloc (0 * sizeof(real));

file_input = fopen("input.in","r");
for (i=0; i < 4; i++)
    fscanf(file_input, "%lf" ,&input_values[i]);

fclose(file_input);
for (i=0; i < 4; i++)
 {
    x_input[i] = input_values[i];
 }

f_val = feval0_(&objective_number, x_input);

FILE *output_out;
output_out = fopen ("output.out","w");
fprintf(output_out,"%30.15f\n",f_val);
fclose(output_out);

}
