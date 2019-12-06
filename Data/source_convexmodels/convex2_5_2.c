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
A[0][0] =   -0.0859;
A[0][1] =    0.1808;
A[0][2] =    1.1179;
A[0][3] =  -34.6750;
A[0][4] =   -0.7402;
A[1][0] =   -0.4768;
A[1][1] =    1.5166;
A[1][2] =   -0.5267;
A[1][3] =    2.3941;
A[1][4] =    0.1376;
A[2][0] =    1.0633;
A[2][1] =   -0.7785;
A[2][2] =    0.2988;
A[2][3] =   21.7019;
A[2][4] =   -0.0612;
A[3][0] =    0.2740;
A[3][1] =    0.9350;
A[3][2] =   -1.8687;
A[3][3] =   33.7391;
A[3][4] =   -0.9762;
 double b[m];
b[0] =   -9.5049;
b[1] =    0.6176;
b[2] =    7.0707;
b[3] =   10.3214;
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
