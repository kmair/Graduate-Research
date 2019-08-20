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
		1.,
		2.};

 real x0comn_[1] = {
		1.0911 };

 real
feval0_(fint *nobj, real *x)
{
	real v[3], rv;


  /***  objective ***/

	v[0] = x[0] * x[0];
	v[1] = 2.5 * v[0];
	v[0] = pow(x[0], 3.);
	if (errno) in_trouble2("pow",x[0],3.);
	v[2] = 1.666666666 * v[0];
	v[1] += v[2];
	v[2] = pow(x[0], 4.);
	if (errno) in_trouble2("pow",x[0],4.);
	v[0] = 1.25 * v[2];
	v[1] += v[0];
	v[0] = pow(x[0], 5.);
	if (errno) in_trouble2("pow",x[0],5.);
	v[1] += v[0];
	v[0] = pow(x[0], 6.);
	if (errno) in_trouble2("pow",x[0],6.);
	v[2] = 0.8333333 * v[0];
	v[1] += v[2];
	v[2] = pow(x[0], 7.);
	if (errno) in_trouble2("pow",x[0],7.);
	v[0] = 0.714285714 * v[2];
	v[1] += v[0];
	v[0] = pow(x[0], 8.);
	if (errno) in_trouble2("pow",x[0],8.);
	v[2] = 0.625 * v[0];
	v[1] += v[2];
	v[2] = pow(x[0], 9.);
	if (errno) in_trouble2("pow",x[0],9.);
	v[0] = 0.555555555 * v[2];
	v[1] += v[0];
	v[0] = pow(x[0], 10.);
	if (errno) in_trouble2("pow",x[0],10.);
	v[1] += v[0];
	v[0] = pow(x[0], 11.);
	if (errno) in_trouble2("pow",x[0],11.);
	v[2] = -43.6363636 * v[0];
	v[1] += v[2];
	v[2] = pow(x[0], 12.);
	if (errno) in_trouble2("pow",x[0],12.);
	v[0] = 0.41666666 * v[2];
	v[1] += v[0];
	v[0] = pow(x[0], 13.);
	if (errno) in_trouble2("pow",x[0],13.);
	v[2] = 0.384615384 * v[0];
	v[1] += v[2];
	v[2] = pow(x[0], 14.);
	if (errno) in_trouble2("pow",x[0],14.);
	v[0] = 0.357142857 * v[2];
	v[1] += v[0];
	v[0] = pow(x[0], 15.);
	if (errno) in_trouble2("pow",x[0],15.);
	v[2] = 0.3333333 * v[0];
	v[1] += v[2];
	v[2] = pow(x[0], 16.);
	if (errno) in_trouble2("pow",x[0],16.);
	v[0] = 0.3125 * v[2];
	v[1] += v[0];
	v[0] = pow(x[0], 17.);
	if (errno) in_trouble2("pow",x[0],17.);
	v[2] = 0.294117647 * v[0];
	v[1] += v[2];
	v[2] = pow(x[0], 18.);
	if (errno) in_trouble2("pow",x[0],18.);
	v[0] = 0.277777777 * v[2];
	v[1] += v[0];
	v[0] = pow(x[0], 19.);
	if (errno) in_trouble2("pow",x[0],19.);
	v[2] = 0.263157894 * v[0];
	v[1] += v[2];
	v[2] = pow(x[0], 20.);
	if (errno) in_trouble2("pow",x[0],20.);
	v[0] = 0.25 * v[2];
	v[1] += v[0];
	v[0] = pow(x[0], 21.);
	if (errno) in_trouble2("pow",x[0],21.);
	v[2] = 0.238095238 * v[0];
	v[1] += v[2];
	v[2] = pow(x[0], 22.);
	if (errno) in_trouble2("pow",x[0],22.);
	v[0] = 0.227272727 * v[2];
	v[1] += v[0];
	v[0] = pow(x[0], 23.);
	if (errno) in_trouble2("pow",x[0],23.);
	v[2] = 0.217391304 * v[0];
	v[1] += v[2];
	v[2] = pow(x[0], 24.);
	if (errno) in_trouble2("pow",x[0],24.);
	v[0] = 0.208333333 * v[2];
	v[1] += v[0];
	v[0] = pow(x[0], 25.);
	if (errno) in_trouble2("pow",x[0],25.);
	v[2] = 0.2 * v[0];
	v[1] += v[2];
	v[2] = pow(x[0], 26.);
	if (errno) in_trouble2("pow",x[0],26.);
	v[0] = 0.192307692 * v[2];
	v[1] += v[0];
	v[0] = pow(x[0], 27.);
	if (errno) in_trouble2("pow",x[0],27.);
	v[2] = 0.185185185 * v[0];
	v[1] += v[2];
	v[2] = pow(x[0], 28.);
	if (errno) in_trouble2("pow",x[0],28.);
	v[0] = 0.178571428 * v[2];
	v[1] += v[0];
	v[0] = pow(x[0], 29.);
	if (errno) in_trouble2("pow",x[0],29.);
	v[2] = 0.344827586 * v[0];
	v[1] += v[2];
	v[2] = pow(x[0], 30.);
	if (errno) in_trouble2("pow",x[0],30.);
	v[0] = 0.6666666 * v[2];
	v[1] += v[0];
	v[0] = pow(x[0], 31.);
	if (errno) in_trouble2("pow",x[0],31.);
	v[2] = -15.48387097 * v[0];
	v[1] += v[2];
	v[2] = pow(x[0], 32.);
	if (errno) in_trouble2("pow",x[0],32.);
	v[0] = 0.15625 * v[2];
	v[1] += v[0];
	v[0] = pow(x[0], 33.);
	if (errno) in_trouble2("pow",x[0],33.);
	v[2] = 0.1515151 * v[0];
	v[1] += v[2];
	v[2] = pow(x[0], 34.);
	if (errno) in_trouble2("pow",x[0],34.);
	v[0] = 0.14705882 * v[2];
	v[1] += v[0];
	v[0] = pow(x[0], 35.);
	if (errno) in_trouble2("pow",x[0],35.);
	v[2] = 0.14285712 * v[0];
	v[1] += v[2];
	v[2] = pow(x[0], 36.);
	if (errno) in_trouble2("pow",x[0],36.);
	v[0] = 0.138888888 * v[2];
	v[1] += v[0];
	v[0] = pow(x[0], 37.);
	if (errno) in_trouble2("pow",x[0],37.);
	v[2] = 0.135135135 * v[0];
	v[1] += v[2];
	v[2] = pow(x[0], 38.);
	if (errno) in_trouble2("pow",x[0],38.);
	v[0] = 0.131578947 * v[2];
	v[1] += v[0];
	v[0] = pow(x[0], 39.);
	if (errno) in_trouble2("pow",x[0],39.);
	v[2] = 0.128205128 * v[0];
	v[1] += v[2];
	v[2] = pow(x[0], 40.);
	if (errno) in_trouble2("pow",x[0],40.);
	v[0] = 0.125 * v[2];
	v[1] += v[0];
	v[0] = pow(x[0], 41.);
	if (errno) in_trouble2("pow",x[0],41.);
	v[2] = 0.121951219 * v[0];
	v[1] += v[2];
	v[2] = pow(x[0], 42.);
	if (errno) in_trouble2("pow",x[0],42.);
	v[0] = 0.119047619 * v[2];
	v[1] += v[0];
	v[0] = pow(x[0], 43.);
	if (errno) in_trouble2("pow",x[0],43.);
	v[2] = 0.116279069 * v[0];
	v[1] += v[2];
	v[2] = pow(x[0], 44.);
	if (errno) in_trouble2("pow",x[0],44.);
	v[0] = 0.113636363 * v[2];
	v[1] += v[0];
	v[0] = pow(x[0], 45.);
	if (errno) in_trouble2("pow",x[0],45.);
	v[2] = 0.1111111 * v[0];
	v[1] += v[2];
	v[2] = pow(x[0], 46.);
	if (errno) in_trouble2("pow",x[0],46.);
	v[0] = 0.108695652 * v[2];
	v[1] += v[0];
	v[0] = pow(x[0], 47.);
	if (errno) in_trouble2("pow",x[0],47.);
	v[2] = 0.106382978 * v[0];
	v[1] += v[2];
	v[2] = pow(x[0], 48.);
	if (errno) in_trouble2("pow",x[0],48.);
	v[0] = 0.208333333 * v[2];
	v[1] += v[0];
	v[0] = pow(x[0], 49.);
	if (errno) in_trouble2("pow",x[0],49.);
	v[2] = 0.408163265 * v[0];
	v[1] += v[2];
	v[2] = pow(x[0], 50.);
	if (errno) in_trouble2("pow",x[0],50.);
	v[0] = 0.8 * v[2];
	v[1] += v[0];
	rv = v[1] + -500.*x[0];

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
