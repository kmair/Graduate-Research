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
A[0][0] =    0.5669;
A[0][1] =   -1.3930;
A[0][2] =   -2.2679;
A[0][3] =   -3.9112;
A[0][4] =   -0.2369;
A[1][0] =    0.5348;
A[1][1] =    0.8729;
A[1][2] =   -2.0132;
A[1][3] =    9.6276;
A[1][4] =    0.5310;
A[2][0] =   -0.2371;
A[2][1] =   -1.9834;
A[2][2] =    1.6164;
A[2][3] =   -9.9555;
A[2][4] =    0.5904;
A[3][0] =    0.3420;
A[3][1] =   -0.9451;
A[3][2] =    0.9736;
A[3][3] =   -1.3099;
A[3][4] =   -0.6263;
 double b[m];
b[0] =    5.3361;
b[1] =   -4.3938;
b[2] =    6.3460;
b[3] =    1.3237;
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
