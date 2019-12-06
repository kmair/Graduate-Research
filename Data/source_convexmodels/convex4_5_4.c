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
A[0][0] =   -0.7572;
A[0][1] =    0.3436;
A[0][2] =   -0.0032;
A[0][3] =    1.4487;
A[0][4] =    0.3832;
A[1][0] =   -0.1709;
A[1][1] =   -0.5408;
A[1][2] =    0.0632;
A[1][3] =    3.1048;
A[1][4] =   -0.2622;
A[2][0] =   -0.7703;
A[2][1] =    0.9417;
A[2][2] =   -1.2391;
A[2][3] =    0.7852;
A[2][4] =    0.4816;
A[3][0] =   -0.4144;
A[3][1] =    1.2947;
A[3][2] =    0.2032;
A[3][3] =   -2.6475;
A[3][4] =    1.5505;
 double b[m];
b[0] =    1.7660;
b[1] =    2.0691;
b[2] =    2.6064;
b[3] =   -0.4434;
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
