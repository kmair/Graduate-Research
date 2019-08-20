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
	120 /* nvar */,
	1 /* nobj */,
	0 /* ncon */,
	0 /* nzc */,
	0 /* densejac */,

	/* objtype (0 = minimize, 1 = maximize) */

	0 };

 real boundc_[1+240+0] /* Infinity, variable bounds, constraint bounds */ = {
		1.7e80,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.,
		0.,
		10.};

 real x0comn_[120] = {
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0.,
		0. };

 real
feval0_(fint *nobj, real *x)
{
	real v[3], rv;


  /***  objective ***/

	v[0] = 0.1 * x[0];
	v[1] = v[0] * x[1];
	v[0] = exp(v[1]);
	if (errno) in_trouble("exp",v[1]);
	v[1] = 0.1 * x[1];
	v[2] = v[1] * x[2];
	v[1] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[0] += v[1];
	v[1] = 0.1 * x[2];
	v[2] = v[1] * x[3];
	v[1] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[0] += v[1];
	v[1] = 0.1 * x[3];
	v[2] = v[1] * x[4];
	v[1] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[0] += v[1];
	v[1] = 0.1 * x[4];
	v[2] = v[1] * x[5];
	v[1] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[0] += v[1];
	v[1] = 0.1 * x[5];
	v[2] = v[1] * x[6];
	v[1] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[0] += v[1];
	v[1] = 0.1 * x[6];
	v[2] = v[1] * x[7];
	v[1] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[0] += v[1];
	v[1] = 0.1 * x[7];
	v[2] = v[1] * x[8];
	v[1] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[0] += v[1];
	v[1] = 0.1 * x[8];
	v[2] = v[1] * x[9];
	v[1] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[0] += v[1];
	v[1] = 0.1 * x[9];
	v[2] = v[1] * x[10];
	v[1] = exp(v[2]);
	if (errno) in_trouble("exp",v[2]);
	v[0] += v[1];
	rv = v[0] + -10.*x[0];
	rv += -20.*x[1];
	rv += -30.*x[2];
	rv += -40.*x[3];
	rv += -50.*x[4];
	rv += -60.*x[5];
	rv += -70.*x[6];
	rv += -80.*x[7];
	rv += -90.*x[8];
	rv += -100.*x[9];
	rv += -110.*x[10];
	rv += -120.*x[11];
	rv += -130.*x[12];
	rv += -140.*x[13];
	rv += -150.*x[14];
	rv += -160.*x[15];
	rv += -170.*x[16];
	rv += -180.*x[17];
	rv += -190.*x[18];
	rv += -200.*x[19];
	rv += -210.*x[20];
	rv += -220.*x[21];
	rv += -230.*x[22];
	rv += -240.*x[23];
	rv += -250.*x[24];
	rv += -260.*x[25];
	rv += -270.*x[26];
	rv += -280.*x[27];
	rv += -290.*x[28];
	rv += -300.*x[29];
	rv += -310.*x[30];
	rv += -320.*x[31];
	rv += -330.*x[32];
	rv += -340.*x[33];
	rv += -350.*x[34];
	rv += -360.*x[35];
	rv += -370.*x[36];
	rv += -380.*x[37];
	rv += -390.*x[38];
	rv += -400.*x[39];
	rv += -410.*x[40];
	rv += -420.*x[41];
	rv += -430.*x[42];
	rv += -440.*x[43];
	rv += -450.*x[44];
	rv += -460.*x[45];
	rv += -470.*x[46];
	rv += -480.*x[47];
	rv += -490.*x[48];
	rv += -500.*x[49];
	rv += -510.*x[50];
	rv += -520.*x[51];
	rv += -530.*x[52];
	rv += -540.*x[53];
	rv += -550.*x[54];
	rv += -560.*x[55];
	rv += -570.*x[56];
	rv += -580.*x[57];
	rv += -590.*x[58];
	rv += -600.*x[59];
	rv += -610.*x[60];
	rv += -620.*x[61];
	rv += -630.*x[62];
	rv += -640.*x[63];
	rv += -650.*x[64];
	rv += -660.*x[65];
	rv += -670.*x[66];
	rv += -680.*x[67];
	rv += -690.*x[68];
	rv += -700.*x[69];
	rv += -710.*x[70];
	rv += -720.*x[71];
	rv += -730.*x[72];
	rv += -740.*x[73];
	rv += -750.*x[74];
	rv += -760.*x[75];
	rv += -770.*x[76];
	rv += -780.*x[77];
	rv += -790.*x[78];
	rv += -800.*x[79];
	rv += -810.*x[80];
	rv += -820.*x[81];
	rv += -830.*x[82];
	rv += -840.*x[83];
	rv += -850.*x[84];
	rv += -860.*x[85];
	rv += -870.*x[86];
	rv += -880.*x[87];
	rv += -890.*x[88];
	rv += -900.*x[89];
	rv += -910.*x[90];
	rv += -920.*x[91];
	rv += -930.*x[92];
	rv += -940.*x[93];
	rv += -950.*x[94];
	rv += -960.*x[95];
	rv += -970.*x[96];
	rv += -980.*x[97];
	rv += -990.*x[98];
	rv += -1000.*x[99];
	rv += -1010.*x[100];
	rv += -1020.*x[101];
	rv += -1030.*x[102];
	rv += -1040.*x[103];
	rv += -1050.*x[104];
	rv += -1060.*x[105];
	rv += -1070.*x[106];
	rv += -1080.*x[107];
	rv += -1090.*x[108];
	rv += -1100.*x[109];
	rv += -1110.*x[110];
	rv += -1120.*x[111];
	rv += -1130.*x[112];
	rv += -1140.*x[113];
	rv += -1150.*x[114];
	rv += -1160.*x[115];
	rv += -1170.*x[116];
	rv += -1180.*x[117];
	rv += -1190.*x[118];
	rv += -1200.*x[119];

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

x_input = malloc (120 * sizeof(real));
input_values = malloc (120 * sizeof(double));
c_val = malloc (0 * sizeof(real));

file_input = fopen("input.in","r");
for (i=0; i < 120; i++)
    fscanf(file_input, "%lf" ,&input_values[i]);

fclose(file_input);
for (i=0; i < 120; i++)
 {
    x_input[i] = input_values[i];
 }

f_val = feval0_(&objective_number, x_input);

FILE *output_out;
output_out = fopen ("output.out","w");
fprintf(output_out,"%30.15f\n",f_val);
fclose(output_out);

}
