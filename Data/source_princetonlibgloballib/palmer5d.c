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
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80};

 real x0comn_[4] = {
		1.,
		1.,
		1.,
		1. };

 real
feval0_(fint *nobj, real *x)
{
	real v[3];


  /***  objective ***/

	v[0] = 83.57418 - x[0];
	v[1] = v[0] * v[0];
	v[0] = 2.4674000736160004 * x[1];
	v[0] += x[0];
	v[2] = 6.088063123280245 * x[2];
	v[0] += v[2];
	v[2] = 15.021687398560534 * x[3];
	v[0] += v[2];
	v[2] = 81.007654 - v[0];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 1.949550365169 * x[1];
	v[0] += x[0];
	v[2] = 3.8007466263305814 * x[2];
	v[0] += v[2];
	v[2] = 7.40974697327763 * x[3];
	v[0] += v[2];
	v[2] = 18.983286 - v[0];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 1.7134731460089998 * x[1];
	v[0] += x[0];
	v[2] = 2.935990222093979 * x[2];
	v[0] += v[2];
	v[2] = 5.0307404025030324 * x[3];
	v[0] += v[2];
	v[2] = 8.051067 - v[0];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 1.4926241929 * x[1];
	v[0] += x[0];
	v[2] = 2.227926981230376 * x[2];
	v[0] += v[2];
	v[2] = 3.3254577121991233 * x[3];
	v[0] += v[2];
	v[2] = 2.044762 - v[0];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 1.2675044472249999 * x[1];
	v[0] += x[0];
	v[2] = 1.6065675237351524 * x[2];
	v[0] += v[2];
	v[2] = 2.0363314811015614 * x[3];
	v[0] += v[2];
	v[2] = -v[0];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 1.0966236512040002 * x[1];
	v[0] += x[0];
	v[2] = 1.2025834323799927 * x[2];
	v[0] += v[2];
	v[2] = 1.3187814344939863 * x[3];
	v[0] += v[2];
	v[2] = 1.170451 - v[0];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 0.7615442022250001 * x[1];
	v[0] += x[0];
	v[2] = 0.5799495719425118 * x[2];
	v[0] += v[2];
	v[2] = 0.44165723409569047 * x[3];
	v[0] += v[2];
	v[2] = 10.479881 - v[0];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 0.487388289424 * x[1];
	v[0] += x[0];
	v[2] = 0.23754734466765276 * x[2];
	v[0] += v[2];
	v[2] = 0.11577779397478062 * x[3];
	v[0] += v[2];
	v[2] = 25.785001 - v[0];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 0.27415591280100005 * x[1];
	v[0] += x[0];
	v[2] = 0.07516146452374954 * x[2];
	v[0] += v[2];
	v[2] = 0.020605959913968536 * x[3];
	v[0] += v[2];
	v[2] = 44.126844 - v[0];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 0.121847072356 * x[1];
	v[0] += x[0];
	v[2] = 0.014846709041728298 * x[2];
	v[0] += v[2];
	v[2] = 0.0018090280308559472 * x[3];
	v[0] += v[2];
	v[2] = 62.822177 - v[0];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 0.030461768089 * x[1];
	v[0] += x[0];
	v[2] = 0.0009279193151080186 * x[2];
	v[0] += v[2];
	v[2] = 2.8266062982124175e-05 * x[3];
	v[0] += v[2];
	v[2] = 77.719674 - v[0];
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
