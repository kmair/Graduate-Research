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
A[0][0] =   -0.8387;
A[0][1] =    1.2932;
A[0][2] =   -0.2690;
A[0][3] =   -3.0748;
A[0][4] =   -0.8761;
A[1][0] =    1.0629;
A[1][1] =   -0.0640;
A[1][2] =    3.0268;
A[1][3] =   -0.1272;
A[1][4] =   -1.1116;
A[2][0] =   -1.6624;
A[2][1] =    0.4409;
A[2][2] =    0.3059;
A[2][3] =    1.4212;
A[2][4] =    4.0919;
A[3][0] =    1.5143;
A[3][1] =   -1.4751;
A[3][2] =    0.1381;
A[3][3] =    2.0521;
A[3][4] =   -3.8762;
 double b[m];
b[0] =    1.4790;
b[1] =    2.3823;
b[2] =    1.1265;
b[3] =   -1.0119;
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
