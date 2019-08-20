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
	8 /* nvar */,
	1 /* nobj */,
	0 /* ncon */,
	0 /* nzc */,
	0 /* densejac */,

	/* objtype (0 = minimize, 1 = maximize) */

	0 };

 real boundc_[1+16+0] /* Infinity, variable bounds, constraint bounds */ = {
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
		0.,
		50.,
		-1.7e80,
		1.7e80};

 real x0comn_[8] = {
		1.,
		1.,
		1.,
		1.,
		1.,
		1.,
		1.,
		1. };

 real
feval0_(fint *nobj, real *x)
{
	real v[4];


  /***  objective ***/

	v[0] = x[0] + x[7];
	v[1] = 10.678659 - v[0];
	v[0] = v[1] * v[1];
	v[1] = 2.4674000736160004 * x[1];
	v[1] += x[0];
	v[2] = 6.088063123280245 * x[2];
	v[1] += v[2];
	v[2] = 15.021687398560534 * x[3];
	v[1] += v[2];
	v[2] = 37.06451259304481 * x[4];
	v[1] += v[2];
	v[2] = 91.45298110061994 * x[5];
	v[1] += v[2];
	v[2] = 2.4674000736160004 * x[6];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[7] * v[2];
	v[1] += v[3];
	v[3] = 75.414511 - v[1];
	v[1] = v[3] * v[3];
	v[0] += v[1];
	v[1] = 1.949550365169 * x[1];
	v[1] += x[0];
	v[3] = 3.8007466263305814 * x[2];
	v[1] += v[3];
	v[3] = 7.40974697327763 * x[3];
	v[1] += v[3];
	v[3] = 14.445674917563295 * x[4];
	v[1] += v[3];
	v[3] = 28.162570810648187 * x[5];
	v[1] += v[3];
	v[3] = 1.949550365169 * x[6];
	v[2] = -v[3];
	v[3] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = x[7] * v[3];
	v[1] += v[2];
	v[2] = 41.513459 - v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = 1.4926241929 * x[1];
	v[1] += x[0];
	v[2] = 2.227926981230376 * x[2];
	v[1] += v[2];
	v[2] = 3.3254577121991233 * x[3];
	v[1] += v[2];
	v[2] = 4.963658633694297 * x[4];
	v[1] += v[2];
	v[2] = 7.408876961949066 * x[5];
	v[1] += v[2];
	v[2] = 1.4926241929 * x[6];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[7] * v[2];
	v[1] += v[3];
	v[3] = 20.104735 - v[1];
	v[1] = v[3] * v[3];
	v[0] += v[1];
	v[1] = 1.0966236512040002 * x[1];
	v[1] += x[0];
	v[3] = 1.2025834323799927 * x[2];
	v[1] += v[3];
	v[3] = 1.3187814344939863 * x[3];
	v[1] += v[3];
	v[3] = 1.4462069118348444 * x[4];
	v[1] += v[3];
	v[3] = 1.5859447040527888 * x[5];
	v[1] += v[3];
	v[3] = 1.0966236512040002 * x[6];
	v[2] = -v[3];
	v[3] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = x[7] * v[3];
	v[1] += v[2];
	v[2] = 7.432436 - v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = 0.7615442022250001 * x[1];
	v[1] += x[0];
	v[2] = 0.5799495719425118 * x[2];
	v[1] += v[2];
	v[2] = 0.44165723409569047 * x[3];
	v[1] += v[2];
	v[2] = 0.33634150599630275 * x[4];
	v[1] += v[2];
	v[2] = 0.25613892385910947 * x[5];
	v[1] += v[2];
	v[2] = 0.7615442022250001 * x[6];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[7] * v[2];
	v[1] += v[3];
	v[3] = 1.298082 - v[1];
	v[1] = v[3] * v[3];
	v[0] += v[1];
	v[1] = 0.6168500184040001 * x[1];
	v[1] += x[0];
	v[3] = 0.3805039452050153 * x[2];
	v[1] += v[3];
	v[3] = 0.23471386560250834 * x[3];
	v[1] += v[3];
	v[3] = 0.1447832523165813 * x[4];
	v[1] += v[3];
	v[3] = 0.08930955185607416 * x[5];
	v[1] += v[3];
	v[3] = 0.6168500184040001 * x[6];
	v[2] = -v[3];
	v[3] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = x[7] * v[3];
	v[1] += v[2];
	v[2] = 0.1713 - v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = 0.536979718521 * x[1];
	v[1] += x[0];
	v[2] = 0.28834721810289243 * x[2];
	v[1] += v[2];
	v[2] = 0.1548366080132046 * x[3];
	v[1] += v[2];
	v[2] = 0.08314411818767702 * x[4];
	v[1] += v[2];
	v[2] = 0.044646705181095564 * x[5];
	v[1] += v[2];
	v[2] = 0.536979718521 * x[6];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[7] * v[2];
	v[1] += v[3];
	v[3] = -v[1];
	v[1] = v[3] * v[3];
	v[0] += v[1];
	v[1] = 0.487388289424 * x[1];
	v[1] += x[0];
	v[3] = 0.23754734466765276 * x[2];
	v[1] += v[3];
	v[3] = 0.11577779397478062 * x[3];
	v[1] += v[3];
	v[3] = 0.056428740958652614 * x[4];
	v[1] += v[3];
	v[3] = 0.0275027075301877 * x[5];
	v[1] += v[3];
	v[3] = 0.487388289424 * x[6];
	v[2] = -v[3];
	v[3] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = x[7] * v[3];
	v[1] += v[2];
	v[2] = 0.068203 - v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = 0.373156048225 * x[1];
	v[1] += x[0];
	v[2] = 0.1392454363268985 * x[2];
	v[1] += v[2];
	v[2] = 0.0519602767531113 * x[3];
	v[1] += v[2];
	v[2] = 0.019389291537868347 * x[4];
	v[1] += v[2];
	v[2] = 0.007235231408153385 * x[5];
	v[1] += v[2];
	v[2] = 0.373156048225 * x[6];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[7] * v[2];
	v[1] += v[3];
	v[3] = 0.774499 - v[1];
	v[1] = v[3] * v[3];
	v[0] += v[1];
	v[1] = 0.27415591280100005 * x[1];
	v[1] += x[0];
	v[3] = 0.07516146452374954 * x[2];
	v[1] += v[3];
	v[3] = 0.020605959913968536 * x[3];
	v[1] += v[3];
	v[3] = 0.005649245749354861 * x[4];
	v[1] += v[3];
	v[3] = 0.0015487741250515515 * x[5];
	v[1] += v[3];
	v[3] = 0.27415591280100005 * x[6];
	v[2] = -v[3];
	v[3] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = x[7] * v[3];
	v[1] += v[2];
	v[2] = 2.070002 - v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = 0.121847072356 * x[1];
	v[1] += x[0];
	v[2] = 0.014846709041728298 * x[2];
	v[1] += v[2];
	v[2] = 0.0018090280308559472 * x[3];
	v[1] += v[2];
	v[2] = 0.00022042476936973677 * x[4];
	v[1] += v[2];
	v[2] = 2.6858112822448926e-05 * x[5];
	v[1] += v[2];
	v[2] = 0.121847072356 * x[6];
	v[3] = -v[2];
	v[2] = exp(v[3]);
	if (errno) in_trouble("exp",v[3]);
	v[3] = x[7] * v[2];
	v[1] += v[3];
	v[3] = 5.574556 - v[1];
	v[1] = v[3] * v[3];
	v[0] += v[1];
	v[1] = 0.030461768089 * x[1];
	v[1] += x[0];
	v[3] = 0.0009279193151080186 * x[2];
	v[1] += v[3];
	v[3] = 2.8266062982124175e-05 * x[3];
	v[1] += v[3];
	v[3] = 8.610342553505343e-07 * x[4];
	v[1] += v[3];
	v[3] = 2.622862580317278e-08 * x[5];
	v[1] += v[3];
	v[3] = 0.030461768089 * x[6];
	v[2] = -v[3];
	v[3] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[2] = x[7] * v[3];
	v[1] += v[2];
	v[2] = 9.026378 - v[1];
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

x_input = malloc (8 * sizeof(real));
input_values = malloc (8 * sizeof(double));
c_val = malloc (0 * sizeof(real));

file_input = fopen("input.in","r");
for (i=0; i < 8; i++)
    fscanf(file_input, "%lf" ,&input_values[i]);

fclose(file_input);
for (i=0; i < 8; i++)
 {
    x_input[i] = input_values[i];
 }

f_val = feval0_(&objective_number, x_input);

FILE *output_out;
output_out = fopen ("output.out","w");
fprintf(output_out,"%30.15f\n",f_val);
fclose(output_out);

}