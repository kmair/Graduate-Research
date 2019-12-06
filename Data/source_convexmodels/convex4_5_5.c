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
A[0][0] =   -0.1465;
A[0][1] =    0.8648;
A[0][2] =    3.1590;
A[0][3] =   -6.2397;
A[0][4] =    0.4204;
A[1][0] =   -0.5585;
A[1][1] =    0.8298;
A[1][2] =   -2.0912;
A[1][3] =   -6.8188;
A[1][4] =    0.6686;
A[2][0] =    0.6463;
A[2][1] =    0.3473;
A[2][2] =   -2.8692;
A[2][3] =   -3.8125;
A[2][4] =   -0.4450;
A[3][0] =   -0.4561;
A[3][1] =   -0.5696;
A[3][2] =   -3.8863;
A[3][3] =    4.0145;
A[3][4] =   -0.8747;
 double b[m];
b[0] =    4.1882;
b[1] =    5.9877;
b[2] =    3.8211;
b[3] =   -1.2032;
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
