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
A[0][0] =   -0.8347;
A[0][1] =   -0.8050;
A[0][2] =   -0.2696;
A[0][3] =   -1.3795;
A[0][4] =    0.6384;
A[1][0] =   -0.7528;
A[1][1] =   -0.4496;
A[1][2] =    0.8867;
A[1][3] =    0.8217;
A[1][4] =   -0.9810;
A[2][0] =    0.3503;
A[2][1] =   -1.5268;
A[2][2] =   -0.6727;
A[2][3] =    0.4686;
A[2][4] =    1.4145;
A[3][0] =   -0.1610;
A[3][1] =    0.6239;
A[3][2] =   -0.3824;
A[3][3] =   -4.5906;
A[3][4] =   -0.6068;
 double b[m];
b[0] =    2.0954;
b[1] =    0.9168;
b[2] =    1.9135;
b[3] =    0.7937;
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
 f_val = f_temp2;
 FILE *file_output;
 file_output = fopen("output.out","w");
 fprintf(file_output, "%30.15f\n" ,f_val);
    fclose(file_output);


 return 0;
 }
