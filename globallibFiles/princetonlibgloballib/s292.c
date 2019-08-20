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
	30 /* nvar */,
	1 /* nobj */,
	0 /* ncon */,
	0 /* nzc */,
	0 /* densejac */,

	/* objtype (0 = minimize, 1 = maximize) */

	0 };

 real boundc_[1+60+0] /* Infinity, variable bounds, constraint bounds */ = {
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

 real x0comn_[30] = {
		1.,
		1.,
		1.,
		1.,
		1.,
		1.,
		1.,
		1.,
		1.,
		1.,
		1.,
		1.,
		1.,
		1.,
		1.,
		1.,
		1.,
		1.,
		1.,
		1.,
		1.,
		1.,
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

	v[0] = x[0] * x[0];
	v[1] = 2. * x[1];
	v[2] = x[1] * v[1];
	v[0] += v[2];
	v[2] = 3. * x[2];
	v[1] = x[2] * v[2];
	v[0] += v[1];
	v[1] = 4. * x[3];
	v[2] = x[3] * v[1];
	v[0] += v[2];
	v[2] = 5. * x[4];
	v[1] = x[4] * v[2];
	v[0] += v[1];
	v[1] = 6. * x[5];
	v[2] = x[5] * v[1];
	v[0] += v[2];
	v[2] = 7. * x[6];
	v[1] = x[6] * v[2];
	v[0] += v[1];
	v[1] = 8. * x[7];
	v[2] = x[7] * v[1];
	v[0] += v[2];
	v[2] = 9. * x[8];
	v[1] = x[8] * v[2];
	v[0] += v[1];
	v[1] = 10. * x[9];
	v[2] = x[9] * v[1];
	v[0] += v[2];
	v[2] = 11. * x[10];
	v[1] = x[10] * v[2];
	v[0] += v[1];
	v[1] = 12. * x[11];
	v[2] = x[11] * v[1];
	v[0] += v[2];
	v[2] = 13. * x[12];
	v[1] = x[12] * v[2];
	v[0] += v[1];
	v[1] = 14. * x[13];
	v[2] = x[13] * v[1];
	v[0] += v[2];
	v[2] = 15. * x[14];
	v[1] = x[14] * v[2];
	v[0] += v[1];
	v[1] = 16. * x[15];
	v[2] = x[15] * v[1];
	v[0] += v[2];
	v[2] = 17. * x[16];
	v[1] = x[16] * v[2];
	v[0] += v[1];
	v[1] = 18. * x[17];
	v[2] = x[17] * v[1];
	v[0] += v[2];
	v[2] = 19. * x[18];
	v[1] = x[18] * v[2];
	v[0] += v[1];
	v[1] = 20. * x[19];
	v[2] = x[19] * v[1];
	v[0] += v[2];
	v[2] = 21. * x[20];
	v[1] = x[20] * v[2];
	v[0] += v[1];
	v[1] = 22. * x[21];
	v[2] = x[21] * v[1];
	v[0] += v[2];
	v[2] = 23. * x[22];
	v[1] = x[22] * v[2];
	v[0] += v[1];
	v[1] = 24. * x[23];
	v[2] = x[23] * v[1];
	v[0] += v[2];
	v[2] = 25. * x[24];
	v[1] = x[24] * v[2];
	v[0] += v[1];
	v[1] = 26. * x[25];
	v[2] = x[25] * v[1];
	v[0] += v[2];
	v[2] = 27. * x[26];
	v[1] = x[26] * v[2];
	v[0] += v[1];
	v[1] = 28. * x[27];
	v[2] = x[27] * v[1];
	v[0] += v[2];
	v[2] = 29. * x[28];
	v[1] = x[28] * v[2];
	v[0] += v[1];
	v[1] = 30. * x[29];
	v[2] = x[29] * v[1];
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

x_input = malloc (30 * sizeof(real));
input_values = malloc (30 * sizeof(double));
c_val = malloc (0 * sizeof(real));

file_input = fopen("input.in","r");
for (i=0; i < 30; i++)
    fscanf(file_input, "%lf" ,&input_values[i]);

fclose(file_input);
for (i=0; i < 30; i++)
 {
    x_input[i] = input_values[i];
 }

f_val = feval0_(&objective_number, x_input);

FILE *output_out;
output_out = fopen ("output.out","w");
fprintf(output_out,"%30.15f\n",f_val);
fclose(output_out);

}
