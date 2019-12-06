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
A[0][0] =    0.4998;
A[0][1] =    1.1660;
A[0][2] =   -2.1219;
A[0][3] =    3.5246;
A[0][4] =    0.0657;
A[1][0] =    1.1149;
A[1][1] =   -0.0559;
A[1][2] =    0.0932;
A[1][3] =    1.7253;
A[1][4] =   -0.2985;
A[2][0] =   -1.6665;
A[2][1] =   -0.2713;
A[2][2] =    0.1180;
A[2][3] =   -1.4512;
A[2][4] =    0.8780;
A[3][0] =    1.7084;
A[3][1] =   -0.2251;
A[3][2] =    3.4803;
A[3][3] =    3.7248;
A[3][4] =    0.7519;
 double b[m];
b[0] =    4.6944;
b[1] =    2.3443;
b[2] =   -1.3168;
b[3] =    2.9515;
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
