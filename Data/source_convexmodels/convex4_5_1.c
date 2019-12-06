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
A[0][0] =   -0.4876;
A[0][1] =   -2.2297;
A[0][2] =    2.6109;
A[0][3] =    3.5728;
A[0][4] =    0.0254;
A[1][0] =    0.7657;
A[1][1] =   -1.0439;
A[1][2] =   -0.9953;
A[1][3] =   -0.9634;
A[1][4] =    0.2699;
A[2][0] =    0.9813;
A[2][1] =    1.9977;
A[2][2] =    2.5475;
A[2][3] =    2.4311;
A[2][4] =   -0.6591;
A[3][0] =    0.0094;
A[3][1] =   -1.5107;
A[3][2] =   -0.8282;
A[3][3] =    0.7574;
A[3][4] =    0.4198;
 double b[m];
b[0] =    3.1864;
b[1] =    0.2809;
b[2] =    1.1892;
b[3] =    1.6148;
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
