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
A[0][0] =    0.5571;
A[0][1] =    0.4698;
A[0][2] =   -0.7919;
A[0][3] =    1.1517;
A[0][4] =    1.5135;
A[0][5] =    0.7518;
A[0][6] =   -4.2516;
A[0][7] =    3.5538;
A[0][8] =    0.2098;
A[0][9] =    0.9299;
A[1][0] =    0.1655;
A[1][1] =    1.2272;
A[1][2] =   -0.9458;
A[1][3] =   -0.0013;
A[1][4] =    0.2318;
A[1][5] =    2.4244;
A[1][6] =   -1.1822;
A[1][7] =    0.7576;
A[1][8] =    0.0425;
A[1][9] =    0.3876;
A[2][0] =    0.4739;
A[2][1] =    0.5590;
A[2][2] =   -0.1462;
A[2][3] =    0.4429;
A[2][4] =   -1.0363;
A[2][5] =   -0.7649;
A[2][6] =   -0.0441;
A[2][7] =    5.0200;
A[2][8] =    0.2812;
A[2][9] =    0.7966;
A[3][0] =    0.4410;
A[3][1] =   -0.8028;
A[3][2] =   -0.9822;
A[3][3] =   -1.1724;
A[3][4] =    1.4520;
A[3][5] =   -0.0773;
A[3][6] =    2.0502;
A[3][7] =    3.4717;
A[3][8] =   -0.0902;
A[3][9] =    0.1430;
A[4][0] =   -0.4366;
A[4][1] =    0.1157;
A[4][2] =   -0.7719;
A[4][3] =   -1.0274;
A[4][4] =   -1.3794;
A[4][5] =   -2.4203;
A[4][6] =   -3.0122;
A[4][7] =    4.2140;
A[4][8] =   -0.8132;
A[4][9] =   -0.8366;
A[5][0] =    0.3220;
A[5][1] =   -0.1137;
A[5][2] =   -0.5293;
A[5][3] =   -0.5611;
A[5][4] =    0.8843;
A[5][5] =   -1.1470;
A[5][6] =   -1.8568;
A[5][7] =   -3.2993;
A[5][8] =   -0.9575;
A[5][9] =   -0.2318;
A[6][0] =    0.1429;
A[6][1] =    0.4588;
A[6][2] =    0.3111;
A[6][3] =   -0.0113;
A[6][4] =   -0.9486;
A[6][5] =   -0.6199;
A[6][6] =    1.1577;
A[6][7] =    3.6309;
A[6][8] =   -0.7373;
A[6][9] =   -0.3081;
A[7][0] =    0.6859;
A[7][1] =    0.9335;
A[7][2] =    0.0631;
A[7][3] =   -0.0526;
A[7][4] =    1.0132;
A[7][5] =    1.0917;
A[7][6] =    2.3081;
A[7][7] =   -5.1149;
A[7][8] =   -0.7432;
A[7][9] =   -0.7905;
 double b[m];
b[0] =   -0.6911;
b[1] =    1.1449;
b[2] =    0.4813;
b[3] =    4.1728;
b[4] =   -3.2129;
b[5] =   -0.5765;
b[6] =    0.9549;
b[7] =    3.4831;
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
