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
A[0][0] =   -1.2363;
A[0][1] =   -0.0681;
A[0][2] =    1.1726;
A[0][3] =    3.1623;
A[0][4] =    0.0863;
A[1][0] =   -0.1108;
A[1][1] =    1.3512;
A[1][2] =    1.7164;
A[1][3] =    0.9851;
A[1][4] =   -0.0843;
A[2][0] =    0.7784;
A[2][1] =   -1.2521;
A[2][2] =    0.1707;
A[2][3] =    2.6314;
A[2][4] =    0.6434;
A[3][0] =   -1.1901;
A[3][1] =    0.1128;
A[3][2] =    2.0080;
A[3][3] =   -1.6134;
A[3][4] =   -0.5331;
 double b[m];
b[0] =    4.2427;
b[1] =    1.3269;
b[2] =    3.6110;
b[3] =    1.4336;
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
