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
 n = 20;
 m = 16;
 double x[n];
 FILE *file_input;
 file_input = fopen("input.in","r");
 for (i = 0; i < n; i++)
 {
 fscanf(file_input, "%lf\n", &x[i]);
 }
 fclose(file_input);
 
 double A[m][n];
A[0][0] =   -0.5976;
A[0][1] =    0.2923;
A[0][2] =    0.7477;
A[0][3] =    0.7029;
A[0][4] =    0.4102;
A[0][5] =    1.3227;
A[0][6] =    0.1093;
A[0][7] =    0.4697;
A[0][8] =   -1.9755;
A[0][9] =    1.1404;
A[0][10] =   -0.7077;
A[0][11] =    1.9583;
A[0][12] =   -0.5792;
A[0][13] =    0.2742;
A[0][14] =   -0.4564;
A[0][15] =    1.1578;
A[0][16] =   -0.2950;
A[0][17] =    2.1305;
A[0][18] =   -0.4150;
A[0][19] =    0.6657;
A[1][0] =    0.2629;
A[1][1] =   -0.6250;
A[1][2] =   -0.6959;
A[1][3] =    0.4752;
A[1][4] =   -0.7596;
A[1][5] =   -0.6080;
A[1][6] =   -1.1409;
A[1][7] =    0.5215;
A[1][8] =    0.8845;
A[1][9] =    0.7499;
A[1][10] =    1.3418;
A[1][11] =    1.5537;
A[1][12] =   -1.4990;
A[1][13] =   -0.5247;
A[1][14] =    3.0791;
A[1][15] =   -3.8742;
A[1][16] =    1.3743;
A[1][17] =   -2.2421;
A[1][18] =   -0.2895;
A[1][19] =    0.5500;
A[2][0] =   -0.4995;
A[2][1] =    0.5520;
A[2][2] =    0.8115;
A[2][3] =    1.2506;
A[2][4] =   -0.0639;
A[2][5] =   -0.6443;
A[2][6] =    0.4803;
A[2][7] =    0.0687;
A[2][8] =   -1.5921;
A[2][9] =    1.9947;
A[2][10] =   -0.0784;
A[2][11] =   -2.7452;
A[2][12] =    2.5217;
A[2][13] =   -3.1757;
A[2][14] =   -4.4071;
A[2][15] =   -3.6012;
A[2][16] =   -0.4552;
A[2][17] =    3.3272;
A[2][18] =    0.4879;
A[2][19] =    0.2159;
A[3][0] =   -0.2931;
A[3][1] =   -0.5377;
A[3][2] =   -0.0356;
A[3][3] =   -1.2067;
A[3][4] =    0.1700;
A[3][5] =    1.5296;
A[3][6] =    1.3234;
A[3][7] =    0.3144;
A[3][8] =   -1.7151;
A[3][9] =   -0.1697;
A[3][10] =    0.1583;
A[3][11] =   -0.3435;
A[3][12] =   -1.1281;
A[3][13] =   -0.0365;
A[3][14] =   -1.0168;
A[3][15] =    2.6506;
A[3][16] =   -1.2301;
A[3][17] =    4.6485;
A[3][18] =   -0.3378;
A[3][19] =   -0.0321;
A[4][0] =    0.1550;
A[4][1] =    0.4100;
A[4][2] =    0.3564;
A[4][3] =    0.0250;
A[4][4] =    1.2886;
A[4][5] =   -0.7156;
A[4][6] =   -0.2223;
A[4][7] =    1.7835;
A[4][8] =   -0.1260;
A[4][9] =   -1.2468;
A[4][10] =    1.0329;
A[4][11] =    2.2749;
A[4][12] =   -0.7195;
A[4][13] =    1.2894;
A[4][14] =    1.8318;
A[4][15] =   -1.3620;
A[4][16] =    1.0957;
A[4][17] =    2.4535;
A[4][18] =    0.4084;
A[4][19] =   -0.8579;
A[5][0] =    0.4973;
A[5][1] =   -0.8187;
A[5][2] =    0.6342;
A[5][3] =   -0.8077;
A[5][4] =   -1.0527;
A[5][5] =    0.1259;
A[5][6] =    0.4476;
A[5][7] =    1.9332;
A[5][8] =    0.4963;
A[5][9] =   -0.2555;
A[5][10] =   -1.0327;
A[5][11] =   -2.7095;
A[5][12] =   -1.0042;
A[5][13] =    0.1760;
A[5][14] =    4.4804;
A[5][15] =   -2.8101;
A[5][16] =   -0.7241;
A[5][17] =   -3.6164;
A[5][18] =    0.0402;
A[5][19] =    0.5383;
A[6][0] =    0.3213;
A[6][1] =    0.8338;
A[6][2] =    0.0473;
A[6][3] =   -0.9805;
A[6][4] =   -0.8404;
A[6][5] =   -1.0952;
A[6][6] =    1.2889;
A[6][7] =   -0.6429;
A[6][8] =    0.9867;
A[6][9] =    1.4523;
A[6][10] =    1.4512;
A[6][11] =   -2.2938;
A[6][12] =   -3.6052;
A[6][13] =   -2.4250;
A[6][14] =   -1.7436;
A[6][15] =    1.4138;
A[6][16] =   -1.4601;
A[6][17] =    0.0754;
A[6][18] =    0.4398;
A[6][19] =    0.5889;
A[7][0] =   -0.0042;
A[7][1] =   -0.0018;
A[7][2] =   -0.0948;
A[7][3] =   -0.3193;
A[7][4] =   -1.2666;
A[7][5] =    1.4111;
A[7][6] =   -0.7622;
A[7][7] =    0.1844;
A[7][8] =   -0.1682;
A[7][9] =   -0.1854;
A[7][10] =    0.8222;
A[7][11] =   -1.8707;
A[7][12] =    3.2207;
A[7][13] =    2.1793;
A[7][14] =   -4.6609;
A[7][15] =   -2.5043;
A[7][16] =    0.0081;
A[7][17] =   -1.4906;
A[7][18] =    0.1668;
A[7][19] =    0.3411;
A[8][0] =   -0.3993;
A[8][1] =   -0.5104;
A[8][2] =   -0.8143;
A[8][3] =    0.0785;
A[8][4] =    0.3309;
A[8][5] =    1.2991;
A[8][6] =   -1.0669;
A[8][7] =    1.3111;
A[8][8] =   -1.8639;
A[8][9] =    0.0554;
A[8][10] =   -2.0120;
A[8][11] =   -3.3808;
A[8][12] =    1.1953;
A[8][13] =    0.9258;
A[8][14] =    1.6665;
A[8][15] =   -0.8115;
A[8][16] =    0.7790;
A[8][17] =   -1.4376;
A[8][18] =   -0.1046;
A[8][19] =    0.2918;
A[9][0] =    0.2622;
A[9][1] =    0.2352;
A[9][2] =    0.1881;
A[9][3] =   -0.2047;
A[9][4] =   -0.6018;
A[9][5] =   -0.1574;
A[9][6] =    1.3947;
A[9][7] =    1.9080;
A[9][8] =   -0.5074;
A[9][9] =   -2.0157;
A[9][10] =    1.8871;
A[9][11] =   -1.6157;
A[9][12] =   -2.8285;
A[9][13] =    1.8805;
A[9][14] =   -4.7880;
A[9][15] =   -2.9901;
A[9][16] =   -1.2370;
A[9][17] =    3.6248;
A[9][18] =   -0.1084;
A[9][19] =    0.8224;
A[10][0] =    0.0883;
A[10][1] =    0.4060;
A[10][2] =   -0.2435;
A[10][3] =   -0.7958;
A[10][4] =   -0.2041;
A[10][5] =   -0.6219;
A[10][6] =   -1.1229;
A[10][7] =   -0.4146;
A[10][8] =   -0.0194;
A[10][9] =    0.1029;
A[10][10] =    2.1413;
A[10][11] =   -0.8363;
A[10][12] =   -1.5953;
A[10][13] =   -1.7741;
A[10][14] =   -1.9953;
A[10][15] =    1.7030;
A[10][16] =    1.0804;
A[10][17] =   -1.3035;
A[10][18] =    0.2713;
A[10][19] =    0.1252;
A[11][0] =   -0.4875;
A[11][1] =    0.3364;
A[11][2] =   -0.2435;
A[11][3] =    0.4786;
A[11][4] =   -1.2297;
A[11][5] =    0.9174;
A[11][6] =   -1.3226;
A[11][7] =   -0.9233;
A[11][8] =    1.1990;
A[11][9] =   -1.6497;
A[11][10] =   -1.0468;
A[11][11] =    0.7627;
A[11][12] =   -0.0669;
A[11][13] =    2.8137;
A[11][14] =    4.8263;
A[11][15] =   -3.6271;
A[11][16] =   -1.2028;
A[11][17] =   -1.5498;
A[11][18] =    0.2344;
A[11][19] =   -0.8748;
A[12][0] =    0.1652;
A[12][1] =    0.7529;
A[12][2] =   -0.4148;
A[12][3] =   -0.1218;
A[12][4] =    0.1353;
A[12][5] =    0.1546;
A[12][6] =    0.7713;
A[12][7] =   -1.4299;
A[12][8] =    0.4158;
A[12][9] =   -1.0000;
A[12][10] =    0.4689;
A[12][11] =   -2.4810;
A[12][12] =    1.3530;
A[12][13] =   -0.3030;
A[12][14] =   -3.4074;
A[12][15] =   -2.2883;
A[12][16] =   -1.6005;
A[12][17] =    3.2461;
A[12][18] =    0.3107;
A[12][19] =    0.0615;
A[13][0] =    0.4824;
A[13][1] =    0.3931;
A[13][2] =    0.3402;
A[13][3] =   -1.2577;
A[13][4] =   -0.1595;
A[13][5] =   -0.8181;
A[13][6] =   -0.0949;
A[13][7] =    0.7360;
A[13][8] =   -0.6402;
A[13][9] =   -0.8191;
A[13][10] =    1.5783;
A[13][11] =   -2.3474;
A[13][12] =   -3.0763;
A[13][13] =    1.0154;
A[13][14] =    3.8742;
A[13][15] =   -1.3158;
A[13][16] =    1.1310;
A[13][17] =    1.3473;
A[13][18] =    0.4424;
A[13][19] =    0.7273;
A[14][0] =    0.1698;
A[14][1] =   -0.4535;
A[14][2] =    0.0937;
A[14][3] =   -0.5451;
A[14][4] =    0.7935;
A[14][5] =   -0.3448;
A[14][6] =    0.7334;
A[14][7] =   -2.0648;
A[14][8] =   -0.7805;
A[14][9] =    0.0848;
A[14][10] =    2.2566;
A[14][11] =   -1.2181;
A[14][12] =   -0.9056;
A[14][13] =    2.6075;
A[14][14] =   -1.4525;
A[14][15] =   -3.4061;
A[14][16] =   -1.5450;
A[14][17] =   -2.2250;
A[14][18] =    0.5102;
A[14][19] =   -0.1368;
A[15][0] =    0.2654;
A[15][1] =   -0.8018;
A[15][2] =   -0.1568;
A[15][3] =    0.2664;
A[15][4] =    0.9825;
A[15][5] =   -1.4186;
A[15][6] =    1.0116;
A[15][7] =   -1.7178;
A[15][8] =    1.1226;
A[15][9] =   -0.3643;
A[15][10] =   -0.3218;
A[15][11] =    2.2515;
A[15][12] =    2.1425;
A[15][13] =    0.5461;
A[15][14] =   -0.0519;
A[15][15] =   -2.8182;
A[15][16] =    1.3630;
A[15][17] =    0.1262;
A[15][18] =   -0.3959;
A[15][19] =    0.3871;
 double b[m];
b[0] =   -0.2313;
b[1] =   -2.1226;
b[2] =   -1.1551;
b[3] =    2.7512;
b[4] =    3.0729;
b[5] =    3.3664;
b[6] =    2.0885;
b[7] =    2.3850;
b[8] =    0.1015;
b[9] =    8.4735;
b[10] =   -0.1017;
b[11] =   -1.7033;
b[12] =    0.8705;
b[13] =    4.3297;
b[14] =    2.4094;
b[15] =   -1.6244;
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
