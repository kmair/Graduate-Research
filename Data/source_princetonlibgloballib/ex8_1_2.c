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
	1 /* nvar */,
	1 /* nobj */,
	0 /* ncon */,
	0 /* nzc */,
	0 /* densejac */,

	/* objtype (0 = minimize, 1 = maximize) */

	0 };

 real boundc_[1+2+0] /* Infinity, variable bounds, constraint bounds */ = {
		1.7e80,
		0.,
		6.28318};

 real x0comn_[1] = {
		0. };

 real
feval0_(fint *nobj, real *x)
{
	real v[3];


  /***  objective ***/

	v[0] = -2.09439333333333 + x[0];
	v[1] = cos(v[0]);
	if (errno) in_trouble("cos",v[0]);
	v[0] = -4.21478541710781 * v[1];
	v[1] = 10.8095222429746 + v[0];
	v[0] = pow(v[1], 6.);
	if (errno) in_trouble2("pow",v[1],6.);
	if (v[0] == 0.) {
	zerdiv_(&v[0]);	}
	v[1] = 588600. / v[0];
	v[0] = -2.09439333333333 + x[0];
	v[2] = cos(v[0]);
	if (errno) in_trouble("cos",v[0]);
	v[0] = -4.21478541710781 * v[2];
	v[2] = 10.8095222429746 + v[0];
	v[0] = pow(v[2], 3.);
	if (errno) in_trouble2("pow",v[2],3.);
	if (v[0] == 0.) {
	zerdiv_(&v[0]);	}
	v[2] = 1079.1 / v[0];
	v[0] = -v[2];
	v[1] += v[0];
	v[0] = cos(x[0]);
	if (errno) in_trouble("cos",x[0]);
	v[2] = -4.21478541710781 * v[0];
	v[0] = 10.8095222429746 + v[2];
	v[2] = pow(v[0], 6.);
	if (errno) in_trouble2("pow",v[0],6.);
	if (v[2] == 0.) {
	zerdiv_(&v[2]);	}
	v[0] = 600800. / v[2];
	v[1] += v[0];
	v[0] = cos(x[0]);
	if (errno) in_trouble("cos",x[0]);
	v[2] = -4.21478541710781 * v[0];
	v[0] = 10.8095222429746 + v[2];
	v[2] = pow(v[0], 3.);
	if (errno) in_trouble2("pow",v[0],3.);
	if (v[2] == 0.) {
	zerdiv_(&v[2]);	}
	v[0] = 1071.5 / v[2];
	v[2] = -v[0];
	v[1] += v[2];
	v[2] = 2.09439333333333 + x[0];
	v[0] = cos(v[2]);
	if (errno) in_trouble("cos",v[2]);
	v[2] = -4.21478541710781 * v[0];
	v[0] = 10.8095222429746 + v[2];
	v[2] = pow(v[0], 6.);
	if (errno) in_trouble2("pow",v[0],6.);
	if (v[2] == 0.) {
	zerdiv_(&v[2]);	}
	v[0] = 481300. / v[2];
	v[1] += v[0];
	v[0] = 2.09439333333333 + x[0];
	v[2] = cos(v[0]);
	if (errno) in_trouble("cos",v[0]);
	v[0] = -4.21478541710781 * v[2];
	v[2] = 10.8095222429746 + v[0];
	v[0] = pow(v[2], 3.);
	if (errno) in_trouble2("pow",v[2],3.);
	if (v[0] == 0.) {
	zerdiv_(&v[0]);	}
	v[2] = 1064.6 / v[0];
	v[0] = -v[2];
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

x_input = malloc (1 * sizeof(real));
input_values = malloc (1 * sizeof(double));
c_val = malloc (0 * sizeof(real));

file_input = fopen("input.in","r");
for (i=0; i < 1; i++)
    fscanf(file_input, "%lf" ,&input_values[i]);

fclose(file_input);
for (i=0; i < 1; i++)
 {
    x_input[i] = input_values[i];
 }

f_val = feval0_(&objective_number, x_input);

FILE *output_out;
output_out = fopen ("output.out","w");
fprintf(output_out,"%30.15f\n",f_val);
fclose(output_out);

}
