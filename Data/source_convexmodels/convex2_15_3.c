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
 n = 15;
 m = 12;
 double x[n];
 FILE *file_input;
 file_input = fopen("input.in","r");
 for (i = 0; i < n; i++)
 {
 fscanf(file_input, "%lf\n", &x[i]);
 }
 fclose(file_input);
 
 double A[m][n];
A[0][0] =    0.3384;
A[0][1] =   -0.2370;
A[0][2] =    1.3624;
A[0][3] =    0.0856;
A[0][4] =    1.0666;
A[0][5] =    0.5376;
A[0][6] =    1.9813;
A[0][7] =    2.5755;
A[0][8] =   -2.9079;
A[0][9] =   -3.1653;
A[0][10] =    3.3058;
A[0][11] =    3.6465;
A[0][12] =   -0.6329;
A[0][13] =   -0.2368;
A[0][14] =   -0.1518;
A[1][0] =    0.9259;
A[1][1] =   -0.4958;
A[1][2] =   -1.2422;
A[1][3] =    2.1001;
A[1][4] =    0.1102;
A[1][5] =    1.3153;
A[1][6] =   -2.3779;
A[1][7] =   -1.6515;
A[1][8] =    1.8325;
A[1][9] =   -0.8851;
A[1][10] =   -5.8848;
A[1][11] =   -1.0870;
A[1][12] =    0.4741;
A[1][13] =    0.1354;
A[1][14] =   -0.3179;
A[2][0] =    0.0218;
A[2][1] =   -0.4684;
A[2][2] =    1.1234;
A[2][3] =   -0.0433;
A[2][4] =    1.3563;
A[2][5] =    1.7946;
A[2][6] =    0.9738;
A[2][7] =   -1.2360;
A[2][8] =    0.5836;
A[2][9] =    2.6061;
A[2][10] =    4.7584;
A[2][11] =   -6.1033;
A[2][12] =    0.3934;
A[2][13] =    0.7757;
A[2][14] =    0.0827;
A[3][0] =    1.0521;
A[3][1] =   -0.9548;
A[3][2] =    1.0114;
A[3][3] =    0.8366;
A[3][4] =    1.4115;
A[3][5] =   -0.5915;
A[3][6] =    1.2192;
A[3][7] =   -0.5949;
A[3][8] =    3.5720;
A[3][9] =   -1.2568;
A[3][10] =    2.4643;
A[3][11] =    1.1809;
A[3][12] =    0.5540;
A[3][13] =    0.6859;
A[3][14] =    0.8523;
A[4][0] =   -0.6717;
A[4][1] =    0.2271;
A[4][2] =    1.4066;
A[4][3] =   -0.3803;
A[4][4] =   -1.3831;
A[4][5] =   -2.0976;
A[4][6] =   -0.3277;
A[4][7] =    1.3580;
A[4][8] =    0.5965;
A[4][9] =   -4.0939;
A[4][10] =   -4.9779;
A[4][11] =   -4.4874;
A[4][12] =    0.0038;
A[4][13] =    0.7976;
A[4][14] =   -0.4030;
A[5][0] =   -0.8627;
A[5][1] =   -0.5259;
A[5][2] =   -1.2209;
A[5][3] =   -1.9973;
A[5][4] =   -1.6760;
A[5][5] =    1.1811;
A[5][6] =   -0.6267;
A[5][7] =   -2.3265;
A[5][8] =    4.0924;
A[5][9] =    4.5841;
A[5][10] =    4.9101;
A[5][11] =    3.1483;
A[5][12] =   -0.1490;
A[5][13] =    0.8780;
A[5][14] =   -0.3238;
A[6][0] =   -0.4496;
A[6][1] =   -0.8346;
A[6][2] =    0.0159;
A[6][3] =   -0.8894;
A[6][4] =    1.4297;
A[6][5] =   -0.8485;
A[6][6] =    2.5017;
A[6][7] =    2.4242;
A[6][8] =   -0.8198;
A[6][9] =    1.0835;
A[6][10] =   -4.1123;
A[6][11] =    0.4722;
A[6][12] =    0.2225;
A[6][13] =    0.6309;
A[6][14] =    0.7190;
A[7][0] =   -0.2298;
A[7][1] =   -1.2087;
A[7][2] =   -0.3199;
A[7][3] =    1.2941;
A[7][4] =    0.6142;
A[7][5] =    1.7158;
A[7][6] =   -0.5268;
A[7][7] =   -2.7214;
A[7][8] =   -1.4451;
A[7][9] =   -1.6267;
A[7][10] =    1.4706;
A[7][11] =   -3.4007;
A[7][12] =    0.7115;
A[7][13] =   -0.9973;
A[7][14] =   -0.3190;
A[8][0] =   -0.1758;
A[8][1] =   -0.5238;
A[8][2] =   -1.0988;
A[8][3] =   -0.6590;
A[8][4] =   -2.1551;
A[8][5] =   -0.6478;
A[8][6] =   -0.3120;
A[8][7] =   -0.2498;
A[8][8] =   -0.1122;
A[8][9] =   -3.5811;
A[8][10] =    0.5897;
A[8][11] =    5.7435;
A[8][12] =    0.3416;
A[8][13] =   -0.9938;
A[8][14] =   -0.7238;
A[9][0] =   -0.4183;
A[9][1] =    0.1230;
A[9][2] =    0.2531;
A[9][3] =   -1.7889;
A[9][4] =    1.7635;
A[9][5] =    1.6575;
A[9][6] =   -1.7899;
A[9][7] =   -3.0658;
A[9][8] =   -2.4857;
A[9][9] =   -3.6159;
A[9][10] =   -4.6261;
A[9][11] =    3.5557;
A[9][12] =    0.0472;
A[9][13] =   -0.8251;
A[9][14] =    0.0156;
A[10][0] =    0.4301;
A[10][1] =    0.8978;
A[10][2] =    0.3575;
A[10][3] =    0.0477;
A[10][4] =    0.0685;
A[10][5] =   -1.6336;
A[10][6] =   -0.9073;
A[10][7] =    1.6530;
A[10][8] =    4.3715;
A[10][9] =    0.2861;
A[10][10] =   -6.7725;
A[10][11] =    5.3300;
A[10][12] =   -0.4024;
A[10][13] =   -0.4785;
A[10][14] =    0.7133;
A[11][0] =   -0.9056;
A[11][1] =   -1.1081;
A[11][2] =   -0.9612;
A[11][3] =   -0.5717;
A[11][4] =    0.1983;
A[11][5] =   -1.8710;
A[11][6] =   -0.9698;
A[11][7] =    1.3060;
A[11][8] =   -0.3386;
A[11][9] =   -0.8567;
A[11][10] =    3.7193;
A[11][11] =   -5.9383;
A[11][12] =    0.4079;
A[11][13] =   -0.9544;
A[11][14] =   -0.2314;
 double b[m];
b[0] =    3.8068;
b[1] =    2.8519;
b[2] =   -0.5055;
b[3] =    2.9169;
b[4] =    4.3401;
b[5] =   -5.3331;
b[6] =    0.0814;
b[7] =    2.2388;
b[8] =    1.5086;
b[9] =    7.6033;
b[10] =    1.3445;
b[11] =   -1.3312;
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
