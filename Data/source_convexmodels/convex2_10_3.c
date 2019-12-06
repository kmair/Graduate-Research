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
 n = 10;
 m = 8;
 double x[n];
 FILE *file_input;
 file_input = fopen("input.in","r");
 for (i = 0; i < n; i++)
 {
 fscanf(file_input, "%lf\n", &x[i]);
 }
 fclose(file_input);
 
 double A[m][n];
A[0][0] =   -0.8123;
A[0][1] =    0.2413;
A[0][2] =   -0.2964;
A[0][3] =    0.2484;
A[0][4] =    2.3081;
A[0][5] =   -1.3713;
A[0][6] =    3.0762;
A[0][7] =   -4.4416;
A[0][8] =   -0.0310;
A[0][9] =   -0.1894;
A[1][0] =    0.6772;
A[1][1] =    0.0609;
A[1][2] =   -1.2558;
A[1][3] =   -0.0741;
A[1][4] =    3.4272;
A[1][5] =    1.2321;
A[1][6] =   -3.4688;
A[1][7] =   -6.2450;
A[1][8] =   -0.6963;
A[1][9] =   -0.7903;
A[2][0] =   -0.3365;
A[2][1] =    0.5358;
A[2][2] =    0.8026;
A[2][3] =    1.9580;
A[2][4] =    1.4212;
A[2][5] =    1.9957;
A[2][6] =   -1.1197;
A[2][7] =    4.1086;
A[2][8] =    0.5639;
A[2][9] =   -0.7754;
A[3][0] =   -0.4282;
A[3][1] =    0.4834;
A[3][2] =   -0.4652;
A[3][3] =    1.4991;
A[3][4] =   -2.8462;
A[3][5] =    0.6664;
A[3][6] =   -1.0966;
A[3][7] =   -4.4469;
A[3][8] =   -0.7988;
A[3][9] =    0.5689;
A[4][0] =    0.5119;
A[4][1] =    0.6563;
A[4][2] =    0.3090;
A[4][3] =    1.1751;
A[4][4] =    1.7230;
A[4][5] =    1.9228;
A[4][6] =    1.5512;
A[4][7] =   -1.6881;
A[4][8] =   -0.4119;
A[4][9] =   -0.4169;
A[5][0] =   -1.0177;
A[5][1] =   -0.4945;
A[5][2] =    0.6911;
A[5][3] =   -2.2498;
A[5][4] =   -3.0143;
A[5][5] =   -2.1242;
A[5][6] =    0.8211;
A[5][7] =    0.7823;
A[5][8] =   -0.5253;
A[5][9] =    0.2071;
A[6][0] =   -0.9385;
A[6][1] =    0.4491;
A[6][2] =   -1.1320;
A[6][3] =   -2.1417;
A[6][4] =   -2.9591;
A[6][5] =    1.8822;
A[6][6] =    2.4259;
A[6][7] =   -4.0952;
A[6][8] =    0.0617;
A[6][9] =    0.9288;
A[7][0] =    0.3490;
A[7][1] =    0.1322;
A[7][2] =   -1.0659;
A[7][3] =   -2.0633;
A[7][4] =    1.0886;
A[7][5] =    3.7698;
A[7][6] =   -1.1095;
A[7][7] =    2.1446;
A[7][8] =   -0.8170;
A[7][9] =   -0.1350;
 double b[m];
b[0] =    1.7367;
b[1] =    6.9483;
b[2] =   -1.1465;
b[3] =    2.6396;
b[4] =   -0.5015;
b[5] =   -0.2883;
b[6] =    2.1894;
b[7] =    1.8491;
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