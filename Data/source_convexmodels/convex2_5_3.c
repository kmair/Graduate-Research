#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

main(int argc, char **argv)
{
 double f_val;

 int i;
 int j;
 int n;
 int m;
 n = 5;
 m = 4;
 double x[n];
 FILE *file_input;
 file_input = fopen("input.in","r");
 for (i = 0; i < n; i++)
 {
 fscanf(file_input, "%lf\n", &x[i]);
 }
 fclose(file_input);
 
 double A[m][n];
A[0][0] =   -0.3515;
A[0][1] =    0.2158;
A[0][2] =    0.4016;
A[0][3] =  -11.4152;
A[0][4] =   -0.5201;
A[1][0] =   -0.5276;
A[1][1] =    0.1350;
A[1][2] =   -0.4904;
A[1][3] =   -7.0016;
A[1][4] =   -0.1655;
A[2][0] =   -0.5416;
A[2][1] =   -0.9611;
A[2][2] =    0.0436;
A[2][3] =  -10.1405;
A[2][4] =   -0.9007;
A[3][0] =    0.5496;
A[3][1] =    0.9556;
A[3][2] =   -0.3231;
A[3][3] =   -8.5094;
A[3][4] =    0.8054;
 double b[m];
b[0] =   10.1222;
b[1] =    6.3357;
b[2] =    8.4508;
b[3] =    6.7387;
 f_val = 0;
 double f_temp;
 double f_temp2;
 f_temp2 = 0;
 for (i=0; i< m; i++)
 {
 f_temp = 0;
 for (j=0; j< n; j++)
 {
 f_temp = f_temp + A[i][j]*x[j];
 }
 f_temp = f_temp - b[i];
 f_temp2 = f_temp2 + f_temp*f_temp;
 }
 f_temp2 = 0.5*f_temp2;
 for (i=0; i< n; i++)
 {
 f_temp2 = f_temp2 + fabs(x[i]);
 }
 f_val = f_temp2;
 FILE *file_output;
 file_output = fopen("output.out","w");
 fprintf(file_output, "%30.15f\n" ,f_val);
    fclose(file_output);


 return 0;
 }
