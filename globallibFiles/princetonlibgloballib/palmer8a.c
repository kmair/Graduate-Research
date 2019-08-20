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
	6 /* nvar */,
	1 /* nobj */,
	0 /* ncon */,
	0 /* nzc */,
	0 /* densejac */,

	/* objtype (0 = minimize, 1 = maximize) */

	0 };

 real boundc_[1+12+0] /* Infinity, variable bounds, constraint bounds */ = {
		1.7e80,
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80,
		-1.7e80,
		1.7e80,
		1.e-05,
		1.7e80,
		1.e-05,
		1.7e80};

 real x0comn_[6] = {
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

	if (x[5] == 0.) {
	zerdiv_(&x[5]);	}
	v[0] = x[4] / x[5];
	v[1] = x[0] + v[0];
	v[0] = 4.757534 - v[1];
	v[1] = v[0] * v[0];
	v[0] = 0.030461768089 * x[1];
	v[0] += x[0];
	v[2] = 0.0009279193151080186 * x[2];
	v[0] += v[2];
	v[2] = 2.8266062982124175e-05 * x[3];
	v[0] += v[2];
	v[2] = 0.030461768089 + x[5];
	if (v[2] == 0.) {
	zerdiv_(&v[2]);	}
	v[3] = x[4] / v[2];
	v[0] += v[3];
	v[3] = 3.121416 - v[0];
	v[0] = v[3] * v[3];
	v[1] += v[0];
	v[0] = 0.09869587728100002 * x[1];
	v[0] += x[0];
	v[3] = 0.009740876192266216 * x[2];
	v[0] += v[3];
	v[3] = 0.0009613843212813211 * x[3];
	v[0] += v[3];
	v[3] = 0.09869587728100002 + x[5];
	if (v[3] == 0.) {
	zerdiv_(&v[3]);	}
	v[2] = x[4] / v[3];
	v[0] += v[2];
	v[2] = 1.207606 - v[0];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 0.190385614224 * x[1];
	v[0] += x[0];
	v[2] = 0.03624668210344975 * x[2];
	v[0] += v[2];
	v[2] = 0.006900846835847349 * x[3];
	v[0] += v[2];
	v[2] = 0.190385614224 + x[5];
	if (v[2] == 0.) {
	zerdiv_(&v[2]);	}
	v[3] = x[4] / v[2];
	v[0] += v[3];
	v[3] = 0.131916 - v[0];
	v[0] = v[3] * v[3];
	v[1] += v[0];
	v[0] = 0.26471436601599996 * x[1];
	v[0] += x[0];
	v[3] = 0.0700736955752528 * x[2];
	v[0] += v[3];
	v[3] = 0.018549513898601225 * x[3];
	v[0] += v[3];
	v[3] = 0.26471436601599996 + x[5];
	if (v[3] == 0.) {
	zerdiv_(&v[3]);	}
	v[2] = x[4] / v[3];
	v[0] += v[2];
	v[2] = -v[0];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 0.373156048225 * x[1];
	v[0] += x[0];
	v[2] = 0.1392454363268985 * x[2];
	v[0] += v[2];
	v[2] = 0.0519602767531113 * x[3];
	v[0] += v[2];
	v[2] = 0.373156048225 + x[5];
	if (v[2] == 0.) {
	zerdiv_(&v[2]);	}
	v[3] = x[4] / v[2];
	v[0] += v[3];
	v[3] = 0.258514 - v[0];
	v[0] = v[3] * v[3];
	v[1] += v[0];
	v[0] = 0.6168500184040001 * x[1];
	v[0] += x[0];
	v[3] = 0.3805039452050153 * x[2];
	v[0] += v[3];
	v[3] = 0.23471386560250834 * x[3];
	v[0] += v[3];
	v[3] = 0.6168500184040001 + x[5];
	if (v[3] == 0.) {
	zerdiv_(&v[3]);	}
	v[2] = x[4] / v[3];
	v[0] += v[2];
	v[2] = 3.380161 - v[0];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 0.9214675247609999 * x[1];
	v[0] += x[0];
	v[2] = 0.849102399189164 * x[2];
	v[0] += v[2];
	v[2] = 0.7824202860494653 * x[3];
	v[0] += v[2];
	v[2] = 0.9214675247609999 + x[5];
	if (v[2] == 0.) {
	zerdiv_(&v[2]);	}
	v[3] = x[4] / v[2];
	v[0] += v[3];
	v[3] = 10.762813 - v[0];
	v[0] = v[3] * v[3];
	v[1] += v[0];
	v[0] = 1.2870085672959999 * x[1];
	v[0] += x[0];
	v[3] = 1.6563910522933023 * x[2];
	v[0] += v[3];
	v[3] = 2.1317894750939166 * x[3];
	v[0] += v[3];
	v[3] = 1.2870085672959999 + x[5];
	if (v[3] == 0.) {
	zerdiv_(&v[3]);	}
	v[2] = x[4] / v[3];
	v[0] += v[2];
	v[2] = 23.745996 - v[0];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 1.7134731460089998 * x[1];
	v[0] += x[0];
	v[2] = 2.935990222093979 * x[2];
	v[0] += v[2];
	v[2] = 5.0307404025030324 * x[3];
	v[0] += v[2];
	v[2] = 1.7134731460089998 + x[5];
	if (v[2] == 0.) {
	zerdiv_(&v[2]);	}
	v[3] = x[4] / v[2];
	v[0] += v[3];
	v[3] = 44.471864 - v[0];
	v[0] = v[3] * v[3];
	v[1] += v[0];
	v[0] = 2.2008612609 * x[1];
	v[0] += x[0];
	v[3] = 4.843790289730338 * x[2];
	v[0] += v[3];
	v[3] = 10.660510404591088 * x[3];
	v[0] += v[3];
	v[3] = 2.2008612609 + x[5];
	if (v[3] == 0.) {
	zerdiv_(&v[3]);	}
	v[2] = x[4] / v[3];
	v[0] += v[2];
	v[2] = 76.541947 - v[0];
	v[0] = v[2] * v[2];
	v[1] += v[0];
	v[0] = 2.4674000736160004 * x[1];
	v[0] += x[0];
	v[2] = 6.088063123280245 * x[2];
	v[0] += v[2];
	v[2] = 15.021687398560534 * x[3];
	v[0] += v[2];
	v[2] = 2.4674000736160004 + x[5];
	if (v[2] == 0.) {
	zerdiv_(&v[2]);	}
	v[3] = x[4] / v[2];
	v[0] += v[3];
	v[3] = 97.874528 - v[0];
	v[0] = v[3] * v[3];
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

x_input = malloc (6 * sizeof(real));
input_values = malloc (6 * sizeof(double));
c_val = malloc (0 * sizeof(real));

file_input = fopen("input.in","r");
for (i=0; i < 6; i++)
    fscanf(file_input, "%lf" ,&input_values[i]);

fclose(file_input);
for (i=0; i < 6; i++)
 {
    x_input[i] = input_values[i];
 }

f_val = feval0_(&objective_number, x_input);

FILE *output_out;
output_out = fopen ("output.out","w");
fprintf(output_out,"%30.15f\n",f_val);
fclose(output_out);

}
