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
		-1.7e80,
		1.7e80,
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
	real v[3];


  /***  objective ***/

	v[0] = 2.749172911969 * x[1];
	v[0] += x[0];
	v[1] = 7.557951699904112 * x[2];
	v[0] += v[1];
	v[1] = 20.77811608334644 * x[3];
	v[0] += v[1];
	v[1] = 57.12263389808345 * x[4];
	v[0] += v[1];
	v[1] = 157.0399977729332 * x[5];
	v[0] += v[1];
	v[1] = 431.73010797302004 * x[6];
	v[0] += v[1];
	v[1] = 1186.9007181208783 * x[7];
	v[0] += v[1];
	v[1] = 67.27625 - v[0];
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
	v[2] = 225.65109230007235 * x[6];
	v[1] += v[2];
	v[2] = 556.7715217527294 * x[7];
	v[1] += v[2];
	v[2] = 52.8537 - v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = 1.949550365169 * x[1];
	v[1] += x[0];
	v[2] = 3.8007466263305814 * x[2];
	v[1] += v[2];
	v[2] = 7.40974697327763 * x[3];
	v[1] += v[2];
	v[2] = 14.445674917563295 * x[4];
	v[1] += v[2];
	v[2] = 28.162570810648187 * x[5];
	v[1] += v[2];
	v[2] = 54.90435020799699 * x[6];
	v[1] += v[2];
	v[2] = 107.0387959973672 * x[7];
	v[1] += v[2];
	v[2] = 30.2718 - v[1];
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
	v[2] = 11.058668995624627 * x[6];
	v[1] += v[2];
	v[2] = 16.506436884142463 * x[7];
	v[1] += v[2];
	v[2] = 14.9888 - v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = 1.0966236512040002 * x[1];
	v[1] += x[0];
	v[2] = 1.2025834323799927 * x[2];
	v[1] += v[2];
	v[2] = 1.3187814344939863 * x[3];
	v[1] += v[2];
	v[2] = 1.4462069118348444 * x[4];
	v[1] += v[2];
	v[2] = 1.5859447040527888 * x[5];
	v[1] += v[2];
	v[2] = 1.7391844719660166 * x[6];
	v[1] += v[2];
	v[2] = 1.9072308257646742 * x[7];
	v[1] += v[2];
	v[2] = 5.5675 - v[1];
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
	v[2] = 0.19506111242905555 * x[6];
	v[1] += v[2];
	v[2] = 0.14854765924990615 * x[7];
	v[1] += v[2];
	v[2] = 0.92603 - v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = 0.549257372161 * x[1];
	v[1] += x[0];
	v[2] = 0.30168366087320725 * x[2];
	v[1] += v[2];
	v[2] = 0.1657019747951281 * x[3];
	v[1] += v[2];
	v[2] = 0.09101303123786032 * x[4];
	v[1] += v[2];
	v[2] = 0.04998957837011417 * x[5];
	v[1] += v[2];
	v[2] = 0.02745714445100527 * x[6];
	v[1] += v[2];
	v[2] = 0.015081039008204138 * x[7];
	v[1] += v[2];
	v[2] = -v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = 0.487388289424 * x[1];
	v[1] += x[0];
	v[2] = 0.23754734466765276 * x[2];
	v[1] += v[2];
	v[2] = 0.11577779397478062 * x[3];
	v[1] += v[2];
	v[2] = 0.056428740958652614 * x[4];
	v[1] += v[2];
	v[2] = 0.0275027075301877 * x[5];
	v[1] += v[2];
	v[2] = 0.013404497577666747 * x[6];
	v[1] += v[2];
	v[2] = 0.006533195144967147 * x[7];
	v[1] += v[2];
	v[2] = 0.085108 - v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = 0.27415591280100005 * x[1];
	v[1] += x[0];
	v[2] = 0.07516146452374954 * x[2];
	v[1] += v[2];
	v[2] = 0.020605959913968536 * x[3];
	v[1] += v[2];
	v[2] = 0.005649245749354861 * x[4];
	v[1] += v[2];
	v[2] = 0.0015487741250515515 * x[5];
	v[1] += v[2];
	v[2] = 0.00042460558397607826 * x[6];
	v[1] += v[2];
	v[2] = 0.00011640813145536341 * x[7];
	v[1] += v[2];
	v[2] = 1.867422 - v[1];
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
	v[2] = 3.2725824164225457e-06 * x[6];
	v[1] += v[2];
	v[2] = 3.987545864848112e-07 * x[7];
	v[1] += v[2];
	v[2] = 5.014768 - v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = 0.030461768089 * x[1];
	v[1] += x[0];
	v[2] = 0.0009279193151080186 * x[2];
	v[1] += v[2];
	v[2] = 2.8266062982124175e-05 * x[3];
	v[1] += v[2];
	v[2] = 8.610342553505343e-07 * x[4];
	v[1] += v[2];
	v[2] = 2.622862580317278e-08 * x[5];
	v[1] += v[2];
	v[2] = 7.989703165094106e-10 * x[6];
	v[1] += v[2];
	v[2] = 2.433804849150459e-11 * x[7];
	v[1] += v[2];
	v[2] = 8.26352 - v[1];
	v[1] = v[2] * v[2];
	v[0] += v[1];
	v[1] = 9.8046208 - x[0];
	v[2] = v[1] * v[1];
	v[0] += v[2];
	v[2] = 0.030461768089 * x[1];
	v[2] += x[0];
	v[1] = 0.0009279193151080186 * x[2];
	v[2] += v[1];
	v[1] = 2.8266062982124175e-05 * x[3];
	v[2] += v[1];
	v[1] = 8.610342553505343e-07 * x[4];
	v[2] += v[1];
	v[1] = 2.622862580317278e-08 * x[5];
	v[2] += v[1];
	v[1] = 7.989703165094106e-10 * x[6];
	v[2] += v[1];
	v[1] = 2.433804849150459e-11 * x[7];
	v[2] += v[1];
	v[1] = 8.26352 - v[2];
	v[2] = v[1] * v[1];
	v[0] += v[2];
	v[2] = 0.121847072356 * x[1];
	v[2] += x[0];
	v[1] = 0.014846709041728298 * x[2];
	v[2] += v[1];
	v[1] = 0.0018090280308559472 * x[3];
	v[2] += v[1];
	v[1] = 0.00022042476936973677 * x[4];
	v[2] += v[1];
	v[1] = 2.6858112822448926e-05 * x[5];
	v[2] += v[1];
	v[1] = 3.2725824164225457e-06 * x[6];
	v[2] += v[1];
	v[1] = 3.987545864848112e-07 * x[7];
	v[2] += v[1];
	v[1] = 5.014768 - v[2];
	v[2] = v[1] * v[1];
	v[0] += v[2];
	v[2] = 0.27415591280100005 * x[1];
	v[2] += x[0];
	v[1] = 0.07516146452374954 * x[2];
	v[2] += v[1];
	v[1] = 0.020605959913968536 * x[3];
	v[2] += v[1];
	v[1] = 0.005649245749354861 * x[4];
	v[2] += v[1];
	v[1] = 0.0015487741250515515 * x[5];
	v[2] += v[1];
	v[1] = 0.00042460558397607826 * x[6];
	v[2] += v[1];
	v[1] = 0.00011640813145536341 * x[7];
	v[2] += v[1];
	v[1] = 1.867422 - v[2];
	v[2] = v[1] * v[1];
	v[0] += v[2];
	v[2] = 0.487388289424 * x[1];
	v[2] += x[0];
	v[1] = 0.23754734466765276 * x[2];
	v[2] += v[1];
	v[1] = 0.11577779397478062 * x[3];
	v[2] += v[1];
	v[1] = 0.056428740958652614 * x[4];
	v[2] += v[1];
	v[1] = 0.0275027075301877 * x[5];
	v[2] += v[1];
	v[1] = 0.013404497577666747 * x[6];
	v[2] += v[1];
	v[1] = 0.006533195144967147 * x[7];
	v[2] += v[1];
	v[1] = 0.085108 - v[2];
	v[2] = v[1] * v[1];
	v[0] += v[2];
	v[2] = 0.549257372161 * x[1];
	v[2] += x[0];
	v[1] = 0.30168366087320725 * x[2];
	v[2] += v[1];
	v[1] = 0.1657019747951281 * x[3];
	v[2] += v[1];
	v[1] = 0.09101303123786032 * x[4];
	v[2] += v[1];
	v[1] = 0.04998957837011417 * x[5];
	v[2] += v[1];
	v[1] = 0.02745714445100527 * x[6];
	v[2] += v[1];
	v[1] = 0.015081039008204138 * x[7];
	v[2] += v[1];
	v[1] = -v[2];
	v[2] = v[1] * v[1];
	v[0] += v[2];
	v[2] = 0.7615442022250001 * x[1];
	v[2] += x[0];
	v[1] = 0.5799495719425118 * x[2];
	v[2] += v[1];
	v[1] = 0.44165723409569047 * x[3];
	v[2] += v[1];
	v[1] = 0.33634150599630275 * x[4];
	v[2] += v[1];
	v[1] = 0.25613892385910947 * x[5];
	v[2] += v[1];
	v[1] = 0.19506111242905555 * x[6];
	v[2] += v[1];
	v[1] = 0.14854765924990615 * x[7];
	v[2] += v[1];
	v[1] = 0.92603 - v[2];
	v[2] = v[1] * v[1];
	v[0] += v[2];
	v[2] = 1.0966236512040002 * x[1];
	v[2] += x[0];
	v[1] = 1.2025834323799927 * x[2];
	v[2] += v[1];
	v[1] = 1.3187814344939863 * x[3];
	v[2] += v[1];
	v[1] = 1.4462069118348444 * x[4];
	v[2] += v[1];
	v[1] = 1.5859447040527888 * x[5];
	v[2] += v[1];
	v[1] = 1.7391844719660166 * x[6];
	v[2] += v[1];
	v[1] = 1.9072308257646742 * x[7];
	v[2] += v[1];
	v[1] = 5.5675 - v[2];
	v[2] = v[1] * v[1];
	v[0] += v[2];
	v[2] = 1.4926241929 * x[1];
	v[2] += x[0];
	v[1] = 2.227926981230376 * x[2];
	v[2] += v[1];
	v[1] = 3.3254577121991233 * x[3];
	v[2] += v[1];
	v[1] = 4.963658633694297 * x[4];
	v[2] += v[1];
	v[1] = 7.408876961949066 * x[5];
	v[2] += v[1];
	v[1] = 11.058668995624627 * x[6];
	v[2] += v[1];
	v[1] = 16.506436884142463 * x[7];
	v[2] += v[1];
	v[1] = 14.9888 - v[2];
	v[2] = v[1] * v[1];
	v[0] += v[2];
	v[2] = 1.949550365169 * x[1];
	v[2] += x[0];
	v[1] = 3.8007466263305814 * x[2];
	v[2] += v[1];
	v[1] = 7.40974697327763 * x[3];
	v[2] += v[1];
	v[1] = 14.445674917563295 * x[4];
	v[2] += v[1];
	v[1] = 28.162570810648187 * x[5];
	v[2] += v[1];
	v[1] = 54.90435020799699 * x[6];
	v[2] += v[1];
	v[1] = 107.0387959973672 * x[7];
	v[2] += v[1];
	v[1] = 30.2718 - v[2];
	v[2] = v[1] * v[1];
	v[0] += v[2];
	v[2] = 2.4674000736160004 * x[1];
	v[2] += x[0];
	v[1] = 6.088063123280245 * x[2];
	v[2] += v[1];
	v[1] = 15.021687398560534 * x[3];
	v[2] += v[1];
	v[1] = 37.06451259304481 * x[4];
	v[2] += v[1];
	v[1] = 91.45298110061994 * x[5];
	v[2] += v[1];
	v[1] = 225.65109230007235 * x[6];
	v[2] += v[1];
	v[1] = 556.7715217527294 * x[7];
	v[2] += v[1];
	v[1] = 52.8537 - v[2];
	v[2] = v[1] * v[1];
	v[0] += v[2];
	v[2] = 2.749172911969 * x[1];
	v[2] += x[0];
	v[1] = 7.557951699904112 * x[2];
	v[2] += v[1];
	v[1] = 20.77811608334644 * x[3];
	v[2] += v[1];
	v[1] = 57.12263389808345 * x[4];
	v[2] += v[1];
	v[1] = 157.0399977729332 * x[5];
	v[2] += v[1];
	v[1] = 431.73010797302004 * x[6];
	v[2] += v[1];
	v[1] = 1186.9007181208783 * x[7];
	v[2] += v[1];
	v[1] = 67.27625 - v[2];
	v[2] = v[1] * v[1];
	v[0] += v[2];

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
