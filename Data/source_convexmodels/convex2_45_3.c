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
 n = 45;
 m = 36;
 double x[n];
 FILE *file_input;
 file_input = fopen("input.in","r");
 for (i = 0; i < n; i++)
 {
 fscanf(file_input, "%lf\n", &x[i]);
 }
 fclose(file_input);
 
 double A[m][n];
A[0][0] =   -0.5305;
A[0][1] =   -0.4687;
A[0][2] =   -0.5974;
A[0][3] =    0.3468;
A[0][4] =    1.1485;
A[0][5] =   -0.8706;
A[0][6] =   -0.9236;
A[0][7] =   -1.1694;
A[0][8] =   -0.6394;
A[0][9] =    1.2145;
A[0][10] =   -0.2662;
A[0][11] =    0.6941;
A[0][12] =   -0.1087;
A[0][13] =    0.4690;
A[0][14] =   -1.4728;
A[0][15] =    0.8366;
A[0][16] =    0.1291;
A[0][17] =    1.0376;
A[0][18] =   -1.3171;
A[0][19] =   -1.4805;
A[0][20] =    2.2768;
A[0][21] =    0.6402;
A[0][22] =   -0.4180;
A[0][23] =   -1.4010;
A[0][24] =   -0.4156;
A[0][25] =    1.9505;
A[0][26] =    0.1092;
A[0][27] =    0.2075;
A[0][28] =    3.2319;
A[0][29] =    1.6253;
A[0][30] =   -3.9226;
A[0][31] =    2.8597;
A[0][32] =    3.6064;
A[0][33] =   -2.8164;
A[0][34] =    1.0501;
A[0][35] =   -1.7491;
A[0][36] =   -0.2626;
A[0][37] =    0.9507;
A[0][38] =    5.0432;
A[0][39] =   -0.6857;
A[0][40] =    0.9187;
A[0][41] =   -0.4731;
A[0][42] =   -0.0255;
A[0][43] =   -0.3610;
A[0][44] =   -0.9339;
A[1][0] =    0.6094;
A[1][1] =    0.5265;
A[1][2] =   -0.7267;
A[1][3] =    0.3441;
A[1][4] =    1.0450;
A[1][5] =   -0.5393;
A[1][6] =   -1.2275;
A[1][7] =   -0.7336;
A[1][8] =   -0.8040;
A[1][9] =   -0.9964;
A[1][10] =   -0.9121;
A[1][11] =    0.4195;
A[1][12] =   -0.1123;
A[1][13] =   -0.8774;
A[1][14] =   -0.9596;
A[1][15] =   -1.3412;
A[1][16] =    0.1816;
A[1][17] =    1.1470;
A[1][18] =   -0.0231;
A[1][19] =    1.8784;
A[1][20] =    1.0583;
A[1][21] =    0.0977;
A[1][22] =    1.2801;
A[1][23] =   -2.8556;
A[1][24] =   -2.1719;
A[1][25] =   -1.5744;
A[1][26] =    2.9616;
A[1][27] =   -0.8732;
A[1][28] =    1.4910;
A[1][29] =    1.2394;
A[1][30] =   -3.4112;
A[1][31] =   -3.2070;
A[1][32] =    0.4203;
A[1][33] =    3.2339;
A[1][34] =    0.7760;
A[1][35] =   -2.1445;
A[1][36] =    0.2245;
A[1][37] =    1.8604;
A[1][38] =    4.9795;
A[1][39] =    0.5215;
A[1][40] =    0.7959;
A[1][41] =   -0.9280;
A[1][42] =    0.0181;
A[1][43] =   -0.8064;
A[1][44] =    0.4400;
A[2][0] =   -0.6559;
A[2][1] =   -0.0278;
A[2][2] =    0.2442;
A[2][3] =   -0.9344;
A[2][4] =   -0.0001;
A[2][5] =   -0.2274;
A[2][6] =    0.4989;
A[2][7] =   -0.1414;
A[2][8] =   -0.0417;
A[2][9] =   -1.1296;
A[2][10] =    0.8775;
A[2][11] =    1.3442;
A[2][12] =    0.4961;
A[2][13] =   -0.8111;
A[2][14] =    0.5468;
A[2][15] =   -0.9201;
A[2][16] =    0.1398;
A[2][17] =   -1.7242;
A[2][18] =   -1.0430;
A[2][19] =    0.3611;
A[2][20] =    0.9577;
A[2][21] =   -0.9217;
A[2][22] =   -1.0929;
A[2][23] =    1.0801;
A[2][24] =    1.4023;
A[2][25] =    0.4002;
A[2][26] =   -2.2249;
A[2][27] =    3.2350;
A[2][28] =    2.8325;
A[2][29] =   -3.6274;
A[2][30] =    3.0284;
A[2][31] =    4.6229;
A[2][32] =    0.9225;
A[2][33] =    1.5104;
A[2][34] =   -2.1369;
A[2][35] =    0.4742;
A[2][36] =    0.1820;
A[2][37] =    1.2910;
A[2][38] =   -2.6161;
A[2][39] =   -0.4483;
A[2][40] =    0.9247;
A[2][41] =    0.5760;
A[2][42] =    0.4260;
A[2][43] =   -0.5168;
A[2][44] =   -0.3972;
A[3][0] =   -0.6238;
A[3][1] =    0.0215;
A[3][2] =    0.8852;
A[3][3] =    0.6287;
A[3][4] =    0.1803;
A[3][5] =    1.1143;
A[3][6] =    0.0963;
A[3][7] =    0.5139;
A[3][8] =    1.3257;
A[3][9] =   -1.1158;
A[3][10] =   -0.0254;
A[3][11] =    0.1596;
A[3][12] =   -0.7822;
A[3][13] =   -1.0555;
A[3][14] =   -0.9325;
A[3][15] =    0.7668;
A[3][16] =   -0.9705;
A[3][17] =   -0.7109;
A[3][18] =   -0.2822;
A[3][19] =   -1.2743;
A[3][20] =   -0.6178;
A[3][21] =   -0.7622;
A[3][22] =   -1.6602;
A[3][23] =    1.1694;
A[3][24] =    2.0922;
A[3][25] =    1.9201;
A[3][26] =    2.6481;
A[3][27] =   -1.0386;
A[3][28] =    1.2999;
A[3][29] =   -3.2648;
A[3][30] =   -3.9571;
A[3][31] =   -1.0179;
A[3][32] =    4.3203;
A[3][33] =    1.2212;
A[3][34] =    1.2684;
A[3][35] =   -2.5321;
A[3][36] =    0.0983;
A[3][37] =    2.2257;
A[3][38] =   -3.4541;
A[3][39] =    0.3893;
A[3][40] =    0.5032;
A[3][41] =    0.0811;
A[3][42] =    0.0275;
A[3][43] =   -0.1593;
A[3][44] =    0.9383;
A[4][0] =   -0.0634;
A[4][1] =    0.8058;
A[4][2] =    0.1067;
A[4][3] =   -0.3873;
A[4][4] =    0.0159;
A[4][5] =    0.0926;
A[4][6] =   -0.7533;
A[4][7] =   -0.1481;
A[4][8] =   -0.1266;
A[4][9] =    0.8201;
A[4][10] =   -0.2683;
A[4][11] =    0.5298;
A[4][12] =   -0.0161;
A[4][13] =    1.4922;
A[4][14] =    0.4996;
A[4][15] =    0.3163;
A[4][16] =   -1.3434;
A[4][17] =    0.1683;
A[4][18] =    0.0262;
A[4][19] =   -2.2972;
A[4][20] =    1.2939;
A[4][21] =   -2.0859;
A[4][22] =   -1.0465;
A[4][23] =   -1.0944;
A[4][24] =    2.0991;
A[4][25] =   -2.3047;
A[4][26] =    1.1651;
A[4][27] =   -2.0138;
A[4][28] =    1.1521;
A[4][29] =   -1.6105;
A[4][30] =   -2.7896;
A[4][31] =    0.4052;
A[4][32] =   -1.2912;
A[4][33] =   -0.7203;
A[4][34] =    0.3941;
A[4][35] =    2.6570;
A[4][36] =    0.1794;
A[4][37] =   -1.9533;
A[4][38] =    4.9951;
A[4][39] =   -0.6342;
A[4][40] =   -0.0687;
A[4][41] =   -0.2631;
A[4][42] =    0.8470;
A[4][43] =   -0.4993;
A[4][44] =    0.6860;
A[5][0] =   -0.3763;
A[5][1] =   -0.2258;
A[5][2] =   -0.3665;
A[5][3] =   -0.6858;
A[5][4] =    1.0692;
A[5][5] =   -0.4311;
A[5][6] =   -1.2034;
A[5][7] =    0.1048;
A[5][8] =    1.1469;
A[5][9] =   -0.9973;
A[5][10] =    1.1876;
A[5][11] =    1.3223;
A[5][12] =    1.0751;
A[5][13] =    0.6626;
A[5][14] =   -1.2928;
A[5][15] =    1.4085;
A[5][16] =    0.9365;
A[5][17] =    0.2980;
A[5][18] =   -1.8623;
A[5][19] =    0.6835;
A[5][20] =   -1.9213;
A[5][21] =    0.8111;
A[5][22] =   -1.2113;
A[5][23] =    0.8645;
A[5][24] =    0.5713;
A[5][25] =    2.0828;
A[5][26] =    2.8735;
A[5][27] =   -1.6102;
A[5][28] =   -0.6474;
A[5][29] =   -2.4360;
A[5][30] =   -0.4587;
A[5][31] =    0.7181;
A[5][32] =   -3.7967;
A[5][33] =    4.2610;
A[5][34] =    2.8318;
A[5][35] =    0.1815;
A[5][36] =   -0.2599;
A[5][37] =   -2.7517;
A[5][38] =    0.6505;
A[5][39] =   -0.1622;
A[5][40] =   -0.9989;
A[5][41] =   -0.9071;
A[5][42] =   -0.7218;
A[5][43] =   -0.5287;
A[5][44] =   -0.8375;
A[6][0] =   -0.4703;
A[6][1] =   -0.6358;
A[6][2] =    0.7544;
A[6][3] =   -0.4316;
A[6][4] =   -1.1000;
A[6][5] =    1.1463;
A[6][6] =    1.0232;
A[6][7] =   -0.0696;
A[6][8] =   -0.1102;
A[6][9] =   -1.0976;
A[6][10] =    0.7853;
A[6][11] =   -0.5220;
A[6][12] =   -0.1710;
A[6][13] =   -0.3447;
A[6][14] =   -1.3101;
A[6][15] =   -0.5901;
A[6][16] =    0.6047;
A[6][17] =    1.5730;
A[6][18] =   -1.3384;
A[6][19] =    1.0489;
A[6][20] =   -1.6888;
A[6][21] =    0.9232;
A[6][22] =   -1.9277;
A[6][23] =    0.4354;
A[6][24] =    1.9289;
A[6][25] =    1.2164;
A[6][26] =    0.4948;
A[6][27] =    1.5054;
A[6][28] =    1.0597;
A[6][29] =    3.2880;
A[6][30] =    4.5230;
A[6][31] =   -0.8736;
A[6][32] =    2.8432;
A[6][33] =    0.1536;
A[6][34] =    1.7807;
A[6][35] =   -0.0091;
A[6][36] =   -0.1644;
A[6][37] =    1.3821;
A[6][38] =   -1.5807;
A[6][39] =    0.7816;
A[6][40] =   -0.3713;
A[6][41] =    0.0196;
A[6][42] =    0.7971;
A[6][43] =    0.5954;
A[6][44] =    0.1693;
A[7][0] =   -0.3996;
A[7][1] =   -0.2948;
A[7][2] =   -0.3514;
A[7][3] =   -0.3270;
A[7][4] =   -0.7656;
A[7][5] =    0.8128;
A[7][6] =    1.1189;
A[7][7] =    0.8387;
A[7][8] =   -0.4798;
A[7][9] =   -0.0627;
A[7][10] =    0.4353;
A[7][11] =    0.7708;
A[7][12] =    0.1695;
A[7][13] =   -1.3107;
A[7][14] =    1.2617;
A[7][15] =    1.0719;
A[7][16] =    1.1552;
A[7][17] =   -1.9323;
A[7][18] =   -0.2490;
A[7][19] =   -0.8800;
A[7][20] =   -1.9286;
A[7][21] =   -0.5031;
A[7][22] =   -0.9419;
A[7][23] =    2.3232;
A[7][24] =   -0.8985;
A[7][25] =   -0.1147;
A[7][26] =    3.2118;
A[7][27] =   -0.3810;
A[7][28] =   -3.1989;
A[7][29] =   -2.5145;
A[7][30] =    1.5457;
A[7][31] =   -2.1636;
A[7][32] =   -2.6317;
A[7][33] =    2.6504;
A[7][34] =    1.8332;
A[7][35] =   -0.3300;
A[7][36] =   -0.1574;
A[7][37] =    1.7110;
A[7][38] =   -1.6333;
A[7][39] =   -0.3057;
A[7][40] =   -0.6987;
A[7][41] =    0.8020;
A[7][42] =    0.8039;
A[7][43] =    0.6558;
A[7][44] =   -0.2473;
A[8][0] =    0.2875;
A[8][1] =   -0.8693;
A[8][2] =   -0.0323;
A[8][3] =   -0.3867;
A[8][4] =    0.3662;
A[8][5] =    1.1416;
A[8][6] =    1.2353;
A[8][7] =    0.9807;
A[8][8] =    0.8870;
A[8][9] =   -1.1554;
A[8][10] =    1.1040;
A[8][11] =    0.3944;
A[8][12] =   -1.3807;
A[8][13] =   -1.1341;
A[8][14] =   -0.6102;
A[8][15] =   -1.2357;
A[8][16] =   -1.2311;
A[8][17] =   -2.0314;
A[8][18] =   -0.5864;
A[8][19] =    0.3776;
A[8][20] =    0.4924;
A[8][21] =    0.0631;
A[8][22] =   -0.5331;
A[8][23] =   -2.7918;
A[8][24] =    1.1204;
A[8][25] =   -3.0859;
A[8][26] =   -0.8609;
A[8][27] =    1.6679;
A[8][28] =   -2.7915;
A[8][29] =   -2.4000;
A[8][30] =   -0.4485;
A[8][31] =    1.1811;
A[8][32] =   -4.2920;
A[8][33] =   -3.1506;
A[8][34] =   -0.4504;
A[8][35] =    2.3972;
A[8][36] =    0.0270;
A[8][37] =   -1.4256;
A[8][38] =    5.1565;
A[8][39] =    0.9753;
A[8][40] =    0.9687;
A[8][41] =    0.1463;
A[8][42] =    0.2496;
A[8][43] =    0.1976;
A[8][44] =   -0.5806;
A[9][0] =    0.2343;
A[9][1] =    0.3770;
A[9][2] =   -0.7851;
A[9][3] =    0.0355;
A[9][4] =   -0.7396;
A[9][5] =   -0.2146;
A[9][6] =   -1.0068;
A[9][7] =   -1.1999;
A[9][8] =    0.2516;
A[9][9] =    0.2558;
A[9][10] =    0.3957;
A[9][11] =   -1.3786;
A[9][12] =    0.1887;
A[9][13] =    0.7023;
A[9][14] =    1.6238;
A[9][15] =    0.1096;
A[9][16] =   -0.4322;
A[9][17] =   -1.2650;
A[9][18] =    0.8992;
A[9][19] =    2.3400;
A[9][20] =   -1.2011;
A[9][21] =   -0.0661;
A[9][22] =    1.0025;
A[9][23] =   -0.3727;
A[9][24] =   -2.3736;
A[9][25] =    2.5133;
A[9][26] =    0.1365;
A[9][27] =   -2.9933;
A[9][28] =   -0.7311;
A[9][29] =    0.5947;
A[9][30] =   -4.0436;
A[9][31] =    1.9919;
A[9][32] =    2.7821;
A[9][33] =    3.5716;
A[9][34] =    2.4648;
A[9][35] =   -2.2094;
A[9][36] =   -0.0811;
A[9][37] =    0.5603;
A[9][38] =   -3.8865;
A[9][39] =   -0.0491;
A[9][40] =    0.7760;
A[9][41] =   -0.9934;
A[9][42] =    0.7889;
A[9][43] =   -0.6982;
A[9][44] =   -0.0672;
A[10][0] =   -0.1783;
A[10][1] =    0.4260;
A[10][2] =    0.6756;
A[10][3] =    0.5660;
A[10][4] =   -0.9807;
A[10][5] =   -0.9232;
A[10][6] =    0.1615;
A[10][7] =   -0.5429;
A[10][8] =   -1.0386;
A[10][9] =   -0.9736;
A[10][10] =   -0.2564;
A[10][11] =   -1.1776;
A[10][12] =   -0.5209;
A[10][13] =   -1.3763;
A[10][14] =    1.6009;
A[10][15] =   -0.5493;
A[10][16] =   -1.0939;
A[10][17] =   -0.5705;
A[10][18] =    1.3668;
A[10][19] =   -2.1168;
A[10][20] =    1.1564;
A[10][21] =   -2.2707;
A[10][22] =   -1.1348;
A[10][23] =    2.7338;
A[10][24] =   -0.9696;
A[10][25] =    0.0819;
A[10][26] =   -2.1369;
A[10][27] =    2.4597;
A[10][28] =   -2.2985;
A[10][29] =    1.5615;
A[10][30] =    1.2077;
A[10][31] =   -1.7093;
A[10][32] =    1.9829;
A[10][33] =    2.9462;
A[10][34] =   -1.3650;
A[10][35] =    0.4856;
A[10][36] =   -0.2743;
A[10][37] =   -1.9495;
A[10][38] =   -2.7761;
A[10][39] =   -0.0667;
A[10][40] =    0.7028;
A[10][41] =   -0.9227;
A[10][42] =    0.4280;
A[10][43] =    0.0191;
A[10][44] =   -0.4155;
A[11][0] =    0.7543;
A[11][1] =   -0.7606;
A[11][2] =    0.1770;
A[11][3] =   -0.3185;
A[11][4] =   -0.1280;
A[11][5] =   -0.0156;
A[11][6] =   -0.1542;
A[11][7] =   -1.0766;
A[11][8] =   -1.1249;
A[11][9] =    1.1233;
A[11][10] =   -0.6187;
A[11][11] =    0.8042;
A[11][12] =   -0.0583;
A[11][13] =    0.3026;
A[11][14] =   -0.3319;
A[11][15] =   -0.9312;
A[11][16] =    1.9140;
A[11][17] =   -1.4666;
A[11][18] =    1.4007;
A[11][19] =   -1.0319;
A[11][20] =   -0.7762;
A[11][21] =   -0.8765;
A[11][22] =    2.2939;
A[11][23] =    1.4580;
A[11][24] =    1.5494;
A[11][25] =   -1.7498;
A[11][26] =    1.6165;
A[11][27] =   -1.5446;
A[11][28] =    1.7782;
A[11][29] =   -4.3058;
A[11][30] =   -2.9129;
A[11][31] =   -3.3697;
A[11][32] =    0.8216;
A[11][33] =   -1.0661;
A[11][34] =    2.5802;
A[11][35] =   -1.1932;
A[11][36] =    0.1347;
A[11][37] =    0.9730;
A[11][38] =    1.9058;
A[11][39] =    0.6772;
A[11][40] =    0.6941;
A[11][41] =   -0.4090;
A[11][42] =    0.9421;
A[11][43] =   -0.6418;
A[11][44] =    0.1799;
A[12][0] =   -0.4657;
A[12][1] =   -0.9014;
A[12][2] =    0.4199;
A[12][3] =    0.1794;
A[12][4] =   -0.4656;
A[12][5] =   -0.3786;
A[12][6] =    0.0961;
A[12][7] =    1.2451;
A[12][8] =   -0.2495;
A[12][9] =   -0.4163;
A[12][10] =   -1.3706;
A[12][11] =   -1.3584;
A[12][12] =    1.3287;
A[12][13] =    1.4553;
A[12][14] =    1.5612;
A[12][15] =    0.7917;
A[12][16] =    1.9417;
A[12][17] =   -1.4016;
A[12][18] =    1.1149;
A[12][19] =   -0.6281;
A[12][20] =    1.2370;
A[12][21] =   -0.6893;
A[12][22] =   -0.1649;
A[12][23] =    0.1096;
A[12][24] =   -1.3265;
A[12][25] =   -3.1148;
A[12][26] =   -0.5330;
A[12][27] =   -3.5547;
A[12][28] =   -2.2842;
A[12][29] =    4.0652;
A[12][30] =   -4.0645;
A[12][31] =   -1.8656;
A[12][32] =   -0.7709;
A[12][33] =    3.1140;
A[12][34] =    2.6576;
A[12][35] =    1.5436;
A[12][36] =   -0.0844;
A[12][37] =   -0.4339;
A[12][38] =    0.2282;
A[12][39] =    0.4446;
A[12][40] =   -0.4060;
A[12][41] =    0.8155;
A[12][42] =   -0.2398;
A[12][43] =    0.0151;
A[12][44] =   -0.0320;
A[13][0] =    0.2244;
A[13][1] =    0.3745;
A[13][2] =   -0.4788;
A[13][3] =    0.1280;
A[13][4] =   -0.0561;
A[13][5] =   -1.1256;
A[13][6] =    0.6526;
A[13][7] =   -0.4039;
A[13][8] =   -0.6610;
A[13][9] =   -0.5580;
A[13][10] =   -0.8070;
A[13][11] =   -1.1079;
A[13][12] =   -1.2014;
A[13][13] =    1.1101;
A[13][14] =   -1.3081;
A[13][15] =    1.0195;
A[13][16] =    1.9987;
A[13][17] =   -1.8233;
A[13][18] =    1.4784;
A[13][19] =   -0.6659;
A[13][20] =    1.5744;
A[13][21] =    2.0719;
A[13][22] =   -0.5360;
A[13][23] =   -0.3969;
A[13][24] =    0.7763;
A[13][25] =    3.0061;
A[13][26] =    0.0404;
A[13][27] =    1.8513;
A[13][28] =   -3.7466;
A[13][29] =    2.8486;
A[13][30] =    2.4976;
A[13][31] =   -1.1368;
A[13][32] =    2.2512;
A[13][33] =    3.3068;
A[13][34] =   -1.1851;
A[13][35] =   -2.9126;
A[13][36] =    0.0724;
A[13][37] =    2.8932;
A[13][38] =    4.7088;
A[13][39] =    0.8535;
A[13][40] =    0.8192;
A[13][41] =   -0.6260;
A[13][42] =    0.5903;
A[13][43] =    0.4763;
A[13][44] =    0.8021;
A[14][0] =   -0.0509;
A[14][1] =    0.0014;
A[14][2] =    0.0958;
A[14][3] =   -0.5441;
A[14][4] =   -0.3589;
A[14][5] =    0.6597;
A[14][6] =   -0.2509;
A[14][7] =   -0.8835;
A[14][8] =    0.6763;
A[14][9] =   -0.1272;
A[14][10] =   -0.1236;
A[14][11] =   -1.2434;
A[14][12] =    1.2688;
A[14][13] =   -0.0008;
A[14][14] =   -0.6056;
A[14][15] =    0.3663;
A[14][16] =    0.5707;
A[14][17] =   -0.5888;
A[14][18] =    1.3612;
A[14][19] =   -0.5328;
A[14][20] =    1.1861;
A[14][21] =   -1.0855;
A[14][22] =   -0.6488;
A[14][23] =    2.4694;
A[14][24] =    2.8968;
A[14][25] =   -2.6265;
A[14][26] =   -0.1336;
A[14][27] =   -1.2438;
A[14][28] =   -1.3552;
A[14][29] =   -3.1560;
A[14][30] =   -2.6696;
A[14][31] =    1.8961;
A[14][32] =    2.6503;
A[14][33] =   -2.5340;
A[14][34] =    2.6877;
A[14][35] =    2.7964;
A[14][36] =   -0.1159;
A[14][37] =   -0.4777;
A[14][38] =   -0.0618;
A[14][39] =    0.0717;
A[14][40] =   -0.4015;
A[14][41] =   -0.2242;
A[14][42] =   -0.2820;
A[14][43] =   -0.3557;
A[14][44] =   -0.4747;
A[15][0] =   -0.0952;
A[15][1] =   -0.4792;
A[15][2] =   -0.2825;
A[15][3] =    0.5444;
A[15][4] =   -0.3048;
A[15][5] =   -0.0222;
A[15][6] =   -0.4919;
A[15][7] =    0.5321;
A[15][8] =    1.3077;
A[15][9] =    0.5136;
A[15][10] =    0.8244;
A[15][11] =   -0.3673;
A[15][12] =    0.8200;
A[15][13] =   -0.9863;
A[15][14] =    1.6749;
A[15][15] =   -0.1771;
A[15][16] =    1.1542;
A[15][17] =    2.0232;
A[15][18] =    1.4955;
A[15][19] =    0.9170;
A[15][20] =   -0.8430;
A[15][21] =   -1.0303;
A[15][22] =   -1.8344;
A[15][23] =   -1.0697;
A[15][24] =    0.4139;
A[15][25] =   -1.7944;
A[15][26] =    1.8198;
A[15][27] =   -3.5857;
A[15][28] =   -1.6467;
A[15][29] =   -2.9260;
A[15][30] =    1.6451;
A[15][31] =   -3.7181;
A[15][32] =   -3.0563;
A[15][33] =    1.1660;
A[15][34] =    2.3294;
A[15][35] =   -0.1858;
A[15][36] =    0.0281;
A[15][37] =   -1.6856;
A[15][38] =    1.6949;
A[15][39] =    0.5409;
A[15][40] =    0.5004;
A[15][41] =   -0.0707;
A[15][42] =    0.6197;
A[15][43] =    0.6621;
A[15][44] =    0.3719;
A[16][0] =   -0.2767;
A[16][1] =    0.0438;
A[16][2] =    0.8617;
A[16][3] =   -0.1865;
A[16][4] =    0.9205;
A[16][5] =   -0.8346;
A[16][6] =    0.1229;
A[16][7] =   -0.5608;
A[16][8] =   -0.6220;
A[16][9] =   -1.2824;
A[16][10] =   -1.0999;
A[16][11] =   -1.0897;
A[16][12] =   -0.2866;
A[16][13] =   -0.1313;
A[16][14] =    0.5458;
A[16][15] =    1.8046;
A[16][16] =   -1.8791;
A[16][17] =    0.9708;
A[16][18] =   -1.2173;
A[16][19] =    1.0837;
A[16][20] =    1.4522;
A[16][21] =   -0.6867;
A[16][22] =   -0.4957;
A[16][23] =   -0.9957;
A[16][24] =   -2.8048;
A[16][25] =   -0.7902;
A[16][26] =    0.3866;
A[16][27] =   -2.4344;
A[16][28] =   -2.3883;
A[16][29] =   -0.5762;
A[16][30] =    2.5328;
A[16][31] =   -0.8935;
A[16][32] =    0.2388;
A[16][33] =    4.4622;
A[16][34] =   -1.7169;
A[16][35] =   -2.3953;
A[16][36] =    0.1793;
A[16][37] =    1.8850;
A[16][38] =   -0.7658;
A[16][39] =    0.5494;
A[16][40] =    0.1486;
A[16][41] =    0.3886;
A[16][42] =    0.5756;
A[16][43] =    0.3063;
A[16][44] =    0.9025;
A[17][0] =    0.2926;
A[17][1] =   -0.1503;
A[17][2] =   -0.7585;
A[17][3] =   -0.6376;
A[17][4] =   -0.2622;
A[17][5] =   -0.4244;
A[17][6] =   -0.2170;
A[17][7] =    0.2403;
A[17][8] =   -0.2449;
A[17][9] =    0.4754;
A[17][10] =    0.4946;
A[17][11] =   -1.3203;
A[17][12] =   -1.3113;
A[17][13] =   -1.4192;
A[17][14] =   -0.3042;
A[17][15] =    1.2525;
A[17][16] =   -0.7347;
A[17][17] =    0.9661;
A[17][18] =    1.4977;
A[17][19] =   -0.4664;
A[17][20] =   -0.3391;
A[17][21] =   -1.3997;
A[17][22] =    1.6430;
A[17][23] =   -0.5201;
A[17][24] =    1.2190;
A[17][25] =    2.3792;
A[17][26] =   -0.7883;
A[17][27] =    2.7799;
A[17][28] =   -2.1336;
A[17][29] =   -3.7980;
A[17][30] =    2.3963;
A[17][31] =    1.9809;
A[17][32] =   -3.1426;
A[17][33] =   -0.9373;
A[17][34] =    1.5120;
A[17][35] =    1.4867;
A[17][36] =   -0.0578;
A[17][37] =    0.6900;
A[17][38] =    4.1645;
A[17][39] =    0.5125;
A[17][40] =    0.3587;
A[17][41] =   -0.5060;
A[17][42] =   -0.9496;
A[17][43] =    0.3556;
A[17][44] =   -0.3829;
A[18][0] =   -0.3853;
A[18][1] =   -0.3998;
A[18][2] =   -0.7263;
A[18][3] =   -0.3812;
A[18][4] =   -0.7344;
A[18][5] =    0.7619;
A[18][6] =    0.0732;
A[18][7] =   -0.6625;
A[18][8] =   -0.7551;
A[18][9] =    0.6417;
A[18][10] =   -0.9294;
A[18][11] =    0.2609;
A[18][12] =   -0.9432;
A[18][13] =    0.1646;
A[18][14] =   -0.9729;
A[18][15] =   -1.1513;
A[18][16] =   -1.6631;
A[18][17] =    1.2430;
A[18][18] =   -1.9602;
A[18][19] =    0.8494;
A[18][20] =    0.0490;
A[18][21] =    1.6741;
A[18][22] =    2.1739;
A[18][23] =   -2.8858;
A[18][24] =   -1.8429;
A[18][25] =   -2.2623;
A[18][26] =    1.8210;
A[18][27] =    1.1956;
A[18][28] =   -0.0663;
A[18][29] =    3.5674;
A[18][30] =    3.8081;
A[18][31] =    1.8253;
A[18][32] =   -4.6705;
A[18][33] =    1.5694;
A[18][34] =   -0.4264;
A[18][35] =    0.2863;
A[18][36] =   -0.1521;
A[18][37] =   -2.9739;
A[18][38] =   -1.9596;
A[18][39] =    0.2294;
A[18][40] =   -0.5577;
A[18][41] =    0.6711;
A[18][42] =   -0.5443;
A[18][43] =    0.9593;
A[18][44] =   -0.3760;
A[19][0] =   -0.5838;
A[19][1] =    0.2701;
A[19][2] =    0.0128;
A[19][3] =    0.5958;
A[19][4] =   -0.8702;
A[19][5] =   -0.8370;
A[19][6] =   -0.5298;
A[19][7] =    1.1143;
A[19][8] =    0.7804;
A[19][9] =   -1.0664;
A[19][10] =   -0.0827;
A[19][11] =    1.3867;
A[19][12] =    1.2085;
A[19][13] =    1.3140;
A[19][14] =    1.5278;
A[19][15] =   -0.4750;
A[19][16] =    0.4125;
A[19][17] =    1.3181;
A[19][18] =   -1.8966;
A[19][19] =    2.2715;
A[19][20] =   -1.6951;
A[19][21] =    1.7578;
A[19][22] =    0.3950;
A[19][23] =   -1.6337;
A[19][24] =   -2.2603;
A[19][25] =    0.1576;
A[19][26] =   -2.1030;
A[19][27] =   -2.0453;
A[19][28] =    1.3236;
A[19][29] =   -2.4367;
A[19][30] =    3.7195;
A[19][31] =   -4.3870;
A[19][32] =    4.1618;
A[19][33] =    2.5719;
A[19][34] =   -1.2324;
A[19][35] =    2.2477;
A[19][36] =   -0.1913;
A[19][37] =    1.5695;
A[19][38] =   -0.8808;
A[19][39] =    0.6718;
A[19][40] =   -0.2247;
A[19][41] =   -0.1985;
A[19][42] =    0.9396;
A[19][43] =   -0.9275;
A[19][44] =    0.3493;
A[20][0] =   -0.5238;
A[20][1] =   -0.8420;
A[20][2] =   -0.2468;
A[20][3] =    0.8118;
A[20][4] =   -0.0967;
A[20][5] =    0.2110;
A[20][6] =   -0.0321;
A[20][7] =   -0.8946;
A[20][8] =    0.0869;
A[20][9] =   -0.2251;
A[20][10] =   -1.2454;
A[20][11] =    1.2332;
A[20][12] =    0.3829;
A[20][13] =    0.1880;
A[20][14] =    0.0785;
A[20][15] =   -0.2623;
A[20][16] =   -0.3545;
A[20][17] =    0.6118;
A[20][18] =    0.1580;
A[20][19] =   -0.9760;
A[20][20] =    2.0203;
A[20][21] =    0.2681;
A[20][22] =    0.9874;
A[20][23] =   -1.8354;
A[20][24] =   -2.7127;
A[20][25] =    1.5757;
A[20][26] =   -1.7722;
A[20][27] =    0.7430;
A[20][28] =   -2.0146;
A[20][29] =    1.5920;
A[20][30] =    0.7961;
A[20][31] =   -3.9713;
A[20][32] =   -3.3026;
A[20][33] =    0.6668;
A[20][34] =   -2.8754;
A[20][35] =   -1.3578;
A[20][36] =   -0.0224;
A[20][37] =   -1.5555;
A[20][38] =   -5.2353;
A[20][39] =   -0.4473;
A[20][40] =    0.7781;
A[20][41] =    0.1560;
A[20][42] =    0.3277;
A[20][43] =   -0.3406;
A[20][44] =    0.8856;
A[21][0] =   -0.7562;
A[21][1] =   -0.2845;
A[21][2] =   -0.7620;
A[21][3] =    0.7082;
A[21][4] =   -0.2791;
A[21][5] =   -1.1616;
A[21][6] =    0.9598;
A[21][7] =   -0.0088;
A[21][8] =   -0.5121;
A[21][9] =    0.4998;
A[21][10] =    0.3851;
A[21][11] =   -0.6918;
A[21][12] =   -0.3639;
A[21][13] =    0.0006;
A[21][14] =    1.4754;
A[21][15] =    1.5790;
A[21][16] =    0.7220;
A[21][17] =   -1.6520;
A[21][18] =   -0.0442;
A[21][19] =    2.1234;
A[21][20] =   -0.1891;
A[21][21] =    1.6315;
A[21][22] =   -1.2375;
A[21][23] =   -1.7780;
A[21][24] =    0.5313;
A[21][25] =    2.3403;
A[21][26] =    1.5905;
A[21][27] =    1.7280;
A[21][28] =   -2.8822;
A[21][29] =    2.9468;
A[21][30] =   -0.4816;
A[21][31] =    0.8297;
A[21][32] =    4.5210;
A[21][33] =   -0.3229;
A[21][34] =    0.1439;
A[21][35] =    2.4439;
A[21][36] =    0.0784;
A[21][37] =    0.5291;
A[21][38] =    1.3304;
A[21][39] =   -0.0645;
A[21][40] =   -0.8598;
A[21][41] =   -0.6919;
A[21][42] =   -0.5432;
A[21][43] =   -0.7267;
A[21][44] =   -0.7643;
A[22][0] =   -0.5850;
A[22][1] =    0.8859;
A[22][2] =   -0.5217;
A[22][3] =    0.8358;
A[22][4] =    0.7893;
A[22][5] =   -0.2615;
A[22][6] =   -0.6704;
A[22][7] =    0.6652;
A[22][8] =    0.4353;
A[22][9] =   -0.1396;
A[22][10] =   -0.7290;
A[22][11] =   -0.7769;
A[22][12] =   -0.5259;
A[22][13] =    0.6499;
A[22][14] =   -0.1907;
A[22][15] =    1.7815;
A[22][16] =   -0.6060;
A[22][17] =    0.1091;
A[22][18] =   -0.5898;
A[22][19] =   -0.1920;
A[22][20] =   -1.1221;
A[22][21] =    0.2132;
A[22][22] =    2.1593;
A[22][23] =   -1.2899;
A[22][24] =    2.6944;
A[22][25] =   -1.2191;
A[22][26] =   -1.0876;
A[22][27] =    3.2804;
A[22][28] =    0.3508;
A[22][29] =    2.4612;
A[22][30] =    3.1512;
A[22][31] =   -2.1072;
A[22][32] =   -0.0104;
A[22][33] =    3.2639;
A[22][34] =    2.1598;
A[22][35] =   -1.3530;
A[22][36] =    0.0070;
A[22][37] =   -0.6046;
A[22][38] =   -2.5164;
A[22][39] =   -0.3154;
A[22][40] =   -0.3696;
A[22][41] =   -0.1566;
A[22][42] =   -0.7448;
A[22][43] =   -0.3134;
A[22][44] =    0.7595;
A[23][0] =   -0.4456;
A[23][1] =   -0.7695;
A[23][2] =    0.1500;
A[23][3] =    0.5949;
A[23][4] =   -0.4722;
A[23][5] =   -0.3726;
A[23][6] =    0.5519;
A[23][7] =    1.1500;
A[23][8] =   -1.0577;
A[23][9] =   -0.3998;
A[23][10] =   -0.8022;
A[23][11] =    1.2517;
A[23][12] =   -1.4242;
A[23][13] =    1.3647;
A[23][14] =   -1.4000;
A[23][15] =    0.1865;
A[23][16] =   -2.0118;
A[23][17] =   -0.9264;
A[23][18] =   -0.7764;
A[23][19] =   -0.2389;
A[23][20] =    1.4043;
A[23][21] =   -0.0772;
A[23][22] =   -1.1024;
A[23][23] =    1.3179;
A[23][24] =   -2.2402;
A[23][25] =    0.3317;
A[23][26] =   -0.2009;
A[23][27] =   -1.1413;
A[23][28] =   -3.2077;
A[23][29] =    2.9008;
A[23][30] =    3.9859;
A[23][31] =   -3.6102;
A[23][32] =    4.2041;
A[23][33] =   -1.3366;
A[23][34] =   -1.6153;
A[23][35] =    2.7970;
A[23][36] =   -0.1467;
A[23][37] =   -2.1044;
A[23][38] =   -0.3636;
A[23][39] =    0.0209;
A[23][40] =   -0.8354;
A[23][41] =    0.1412;
A[23][42] =    0.4676;
A[23][43] =   -0.9195;
A[23][44] =    0.4090;
A[24][0] =    0.2191;
A[24][1] =   -0.2369;
A[24][2] =    0.0030;
A[24][3] =   -0.1670;
A[24][4] =    0.8186;
A[24][5] =   -1.0405;
A[24][6] =   -0.4817;
A[24][7] =   -0.6597;
A[24][8] =   -0.8187;
A[24][9] =    0.2448;
A[24][10] =    0.0870;
A[24][11] =    1.1918;
A[24][12] =    0.9089;
A[24][13] =   -1.1656;
A[24][14] =   -0.8947;
A[24][15] =   -0.6142;
A[24][16] =    2.0122;
A[24][17] =    0.8932;
A[24][18] =   -0.4539;
A[24][19] =   -1.2194;
A[24][20] =   -2.0934;
A[24][21] =    0.6557;
A[24][22] =   -0.0597;
A[24][23] =   -1.1067;
A[24][24] =   -1.3941;
A[24][25] =   -2.2639;
A[24][26] =   -0.6011;
A[24][27] =    2.1429;
A[24][28] =   -2.0564;
A[24][29] =    1.5498;
A[24][30] =    3.8185;
A[24][31] =   -3.0622;
A[24][32] =    2.6701;
A[24][33] =   -0.5598;
A[24][34] =   -0.6216;
A[24][35] =   -2.4242;
A[24][36] =   -0.0890;
A[24][37] =    1.2152;
A[24][38] =   -2.0147;
A[24][39] =    0.8087;
A[24][40] =    0.0476;
A[24][41] =    0.0116;
A[24][42] =    0.3022;
A[24][43] =   -0.4947;
A[24][44] =    0.8612;
A[25][0] =   -0.0957;
A[25][1] =    0.6828;
A[25][2] =   -0.4058;
A[25][3] =   -0.3730;
A[25][4] =    1.0605;
A[25][5] =   -1.0188;
A[25][6] =    0.5312;
A[25][7] =    0.4778;
A[25][8] =   -0.9471;
A[25][9] =   -0.1374;
A[25][10] =   -1.2391;
A[25][11] =    1.4355;
A[25][12] =    0.0273;
A[25][13] =    1.4596;
A[25][14] =    0.8698;
A[25][15] =    1.4767;
A[25][16] =    1.7860;
A[25][17] =   -1.1299;
A[25][18] =   -1.1145;
A[25][19] =    1.3280;
A[25][20] =   -1.6356;
A[25][21] =    1.3975;
A[25][22] =    0.0598;
A[25][23] =   -1.7091;
A[25][24] =   -0.0709;
A[25][25] =   -1.1521;
A[25][26] =    0.2398;
A[25][27] =   -2.2171;
A[25][28] =   -3.4390;
A[25][29] =    1.0269;
A[25][30] =   -1.0080;
A[25][31] =    1.1894;
A[25][32] =    1.6256;
A[25][33] =   -0.3921;
A[25][34] =    0.7132;
A[25][35] =   -2.3462;
A[25][36] =    0.2150;
A[25][37] =    1.9977;
A[25][38] =    2.0573;
A[25][39] =    0.0895;
A[25][40] =   -0.1853;
A[25][41] =    0.6967;
A[25][42] =   -0.3191;
A[25][43] =    0.1713;
A[25][44] =   -0.1728;
A[26][0] =   -0.3751;
A[26][1] =   -0.0391;
A[26][2] =    0.0611;
A[26][3] =   -0.3075;
A[26][4] =   -1.1818;
A[26][5] =    0.0378;
A[26][6] =   -1.1158;
A[26][7] =   -1.2332;
A[26][8] =   -0.6427;
A[26][9] =    0.5601;
A[26][10] =   -0.0910;
A[26][11] =    0.4771;
A[26][12] =    0.4718;
A[26][13] =   -0.0238;
A[26][14] =   -0.2333;
A[26][15] =    0.7720;
A[26][16] =    0.3341;
A[26][17] =   -1.5129;
A[26][18] =    0.6106;
A[26][19] =    1.5904;
A[26][20] =    0.6497;
A[26][21] =    1.4532;
A[26][22] =   -0.9827;
A[26][23] =   -1.7893;
A[26][24] =   -1.4985;
A[26][25] =    0.7609;
A[26][26] =    3.0255;
A[26][27] =    0.6281;
A[26][28] =    0.9211;
A[26][29] =    1.5412;
A[26][30] =    3.1364;
A[26][31] =   -2.1799;
A[26][32] =   -1.7680;
A[26][33] =    0.2162;
A[26][34] =   -2.5664;
A[26][35] =    0.9042;
A[26][36] =    0.3038;
A[26][37] =   -2.4122;
A[26][38] =   -5.8832;
A[26][39] =   -0.2302;
A[26][40] =   -0.4829;
A[26][41] =    0.5436;
A[26][42] =   -0.5984;
A[26][43] =    0.2287;
A[26][44] =   -0.7614;
A[27][0] =    0.5661;
A[27][1] =   -0.7531;
A[27][2] =    0.6477;
A[27][3] =   -0.0186;
A[27][4] =    0.3202;
A[27][5] =   -0.4788;
A[27][6] =    1.1647;
A[27][7] =    1.0569;
A[27][8] =    0.8916;
A[27][9] =   -1.3763;
A[27][10] =   -1.2839;
A[27][11] =    0.1621;
A[27][12] =    1.4033;
A[27][13] =   -0.9593;
A[27][14] =    0.3380;
A[27][15] =    1.8554;
A[27][16] =   -0.5056;
A[27][17] =   -1.0053;
A[27][18] =   -1.4465;
A[27][19] =   -0.2003;
A[27][20] =    0.2327;
A[27][21] =   -0.3321;
A[27][22] =   -0.1282;
A[27][23] =    0.7195;
A[27][24] =   -1.7044;
A[27][25] =    2.8515;
A[27][26] =    3.2360;
A[27][27] =    2.8479;
A[27][28] =   -3.3795;
A[27][29] =   -1.6100;
A[27][30] =   -0.8982;
A[27][31] =   -2.9321;
A[27][32] =   -1.4096;
A[27][33] =    0.9324;
A[27][34] =   -1.7960;
A[27][35] =    2.1719;
A[27][36] =   -0.2671;
A[27][37] =   -1.6178;
A[27][38] =    4.7712;
A[27][39] =    0.9468;
A[27][40] =   -0.9833;
A[27][41] =    0.8287;
A[27][42] =    0.0821;
A[27][43] =    0.8753;
A[27][44] =   -0.6950;
A[28][0] =   -0.6759;
A[28][1] =    0.8901;
A[28][2] =   -0.2879;
A[28][3] =    0.6760;
A[28][4] =    0.9883;
A[28][5] =    0.3355;
A[28][6] =   -0.0603;
A[28][7] =    1.0308;
A[28][8] =   -1.0300;
A[28][9] =    0.0891;
A[28][10] =    1.0597;
A[28][11] =    1.0100;
A[28][12] =    1.2222;
A[28][13] =    0.2960;
A[28][14] =    0.1416;
A[28][15] =    1.7255;
A[28][16] =   -0.4383;
A[28][17] =   -1.1655;
A[28][18] =   -0.6630;
A[28][19] =   -0.3687;
A[28][20] =    1.7784;
A[28][21] =    0.8995;
A[28][22] =   -2.3781;
A[28][23] =   -0.8188;
A[28][24] =   -0.0510;
A[28][25] =    0.8456;
A[28][26] =    1.0424;
A[28][27] =    3.0701;
A[28][28] =    0.1873;
A[28][29] =   -0.5663;
A[28][30] =   -4.4595;
A[28][31] =   -1.0185;
A[28][32] =   -1.5692;
A[28][33] =    2.8282;
A[28][34] =   -1.9237;
A[28][35] =   -1.5610;
A[28][36] =   -0.0459;
A[28][37] =    0.9602;
A[28][38] =    1.1356;
A[28][39] =   -0.9872;
A[28][40] =   -0.0720;
A[28][41] =    0.0286;
A[28][42] =    0.1001;
A[28][43] =    0.1552;
A[28][44] =    0.0311;
A[29][0] =    0.0518;
A[29][1] =   -0.2807;
A[29][2] =    0.2979;
A[29][3] =   -0.7786;
A[29][4] =   -1.1753;
A[29][5] =   -0.8155;
A[29][6] =   -0.6986;
A[29][7] =   -0.4407;
A[29][8] =   -0.6612;
A[29][9] =    0.1119;
A[29][10] =    1.2547;
A[29][11] =    0.6576;
A[29][12] =   -0.8568;
A[29][13] =    0.8499;
A[29][14] =    0.7381;
A[29][15] =    1.2139;
A[29][16] =    0.2507;
A[29][17] =    0.1601;
A[29][18] =    1.4721;
A[29][19] =    0.6625;
A[29][20] =   -1.3945;
A[29][21] =   -0.9497;
A[29][22] =   -0.2645;
A[29][23] =    1.2822;
A[29][24] =   -2.6457;
A[29][25] =    3.3264;
A[29][26] =    1.9879;
A[29][27] =    0.7831;
A[29][28] =   -0.0412;
A[29][29] =   -1.4227;
A[29][30] =   -0.7768;
A[29][31] =    2.3245;
A[29][32] =   -0.8795;
A[29][33] =   -4.3926;
A[29][34] =   -0.2637;
A[29][35] =    0.3426;
A[29][36] =    0.2765;
A[29][37] =    1.2684;
A[29][38] =    4.2857;
A[29][39] =   -0.5158;
A[29][40] =   -0.7077;
A[29][41] =   -0.0037;
A[29][42] =   -0.3566;
A[29][43] =   -0.4240;
A[29][44] =    0.3654;
A[30][0] =    0.6210;
A[30][1] =   -0.1906;
A[30][2] =    0.4800;
A[30][3] =    0.1240;
A[30][4] =   -0.8879;
A[30][5] =   -0.3089;
A[30][6] =   -0.0058;
A[30][7] =    0.9495;
A[30][8] =    0.6666;
A[30][9] =    0.6703;
A[30][10] =   -1.2867;
A[30][11] =   -0.7435;
A[30][12] =   -0.4756;
A[30][13] =    0.1593;
A[30][14] =    0.1306;
A[30][15] =   -0.0816;
A[30][16] =    1.9368;
A[30][17] =    0.5833;
A[30][18] =    1.7268;
A[30][19] =    1.0082;
A[30][20] =   -1.7497;
A[30][21] =   -2.3873;
A[30][22] =   -1.7717;
A[30][23] =   -0.5547;
A[30][24] =    1.4549;
A[30][25] =   -2.7067;
A[30][26] =   -3.1255;
A[30][27] =    0.8122;
A[30][28] =   -2.8178;
A[30][29] =   -3.5695;
A[30][30] =    0.6770;
A[30][31] =    3.9773;
A[30][32] =   -3.1744;
A[30][33] =   -0.8894;
A[30][34] =   -2.3043;
A[30][35] =    2.8869;
A[30][36] =    0.2380;
A[30][37] =   -1.2660;
A[30][38] =    2.3804;
A[30][39] =   -0.6788;
A[30][40] =   -0.5510;
A[30][41] =   -0.2715;
A[30][42] =   -0.8877;
A[30][43] =   -0.9985;
A[30][44] =   -0.2726;
A[31][0] =    0.0864;
A[31][1] =    0.2288;
A[31][2] =   -0.2917;
A[31][3] =   -0.4836;
A[31][4] =    0.5301;
A[31][5] =    0.8179;
A[31][6] =   -0.2456;
A[31][7] =    0.2188;
A[31][8] =    1.3414;
A[31][9] =    1.0756;
A[31][10] =    1.0251;
A[31][11] =    0.6484;
A[31][12] =   -0.4971;
A[31][13] =   -1.0275;
A[31][14] =    0.8771;
A[31][15] =    0.3383;
A[31][16] =    0.5373;
A[31][17] =    0.2552;
A[31][18] =    2.0575;
A[31][19] =    1.4728;
A[31][20] =    1.0373;
A[31][21] =   -2.3520;
A[31][22] =    2.3098;
A[31][23] =    2.5217;
A[31][24] =    0.1181;
A[31][25] =   -3.0788;
A[31][26] =    1.2360;
A[31][27] =    2.2495;
A[31][28] =   -1.5329;
A[31][29] =   -3.4157;
A[31][30] =   -2.8727;
A[31][31] =    4.0403;
A[31][32] =   -3.0577;
A[31][33] =   -1.7261;
A[31][34] =    0.7450;
A[31][35] =   -1.4809;
A[31][36] =    0.1786;
A[31][37] =   -1.4051;
A[31][38] =   -3.7352;
A[31][39] =    0.0347;
A[31][40] =   -0.9283;
A[31][41] =    0.3501;
A[31][42] =    0.2705;
A[31][43] =    0.4786;
A[31][44] =    0.7250;
A[32][0] =    0.5836;
A[32][1] =    0.0336;
A[32][2] =    0.9365;
A[32][3] =   -0.7415;
A[32][4] =    0.7261;
A[32][5] =    0.0584;
A[32][6] =    0.6535;
A[32][7] =    0.4210;
A[32][8] =   -0.9535;
A[32][9] =   -0.6865;
A[32][10] =    0.3596;
A[32][11] =   -0.7502;
A[32][12] =    0.7360;
A[32][13] =    0.9285;
A[32][14] =    1.5526;
A[32][15] =   -1.0474;
A[32][16] =    1.1493;
A[32][17] =    1.9214;
A[32][18] =   -0.7025;
A[32][19] =   -2.1949;
A[32][20] =   -1.0293;
A[32][21] =    2.0803;
A[32][22] =   -0.3698;
A[32][23] =    2.8174;
A[32][24] =   -0.5486;
A[32][25] =   -3.1800;
A[32][26] =   -2.0322;
A[32][27] =    3.2321;
A[32][28] =   -2.5662;
A[32][29] =    0.6051;
A[32][30] =    2.3480;
A[32][31] =   -0.3815;
A[32][32] =   -1.2773;
A[32][33] =    0.8441;
A[32][34] =    2.5695;
A[32][35] =   -1.2806;
A[32][36] =   -0.3000;
A[32][37] =   -1.0452;
A[32][38] =   -5.7425;
A[32][39] =    0.5818;
A[32][40] =    0.8851;
A[32][41] =   -0.4300;
A[32][42] =    0.3829;
A[32][43] =   -0.7181;
A[32][44] =   -0.2960;
A[33][0] =   -0.0389;
A[33][1] =   -0.1927;
A[33][2] =    0.1982;
A[33][3] =   -0.7729;
A[33][4] =    0.7757;
A[33][5] =   -0.6236;
A[33][6] =    0.2190;
A[33][7] =    0.4703;
A[33][8] =    0.4380;
A[33][9] =    0.7387;
A[33][10] =    0.7743;
A[33][11] =   -0.8654;
A[33][12] =   -0.8795;
A[33][13] =   -1.3197;
A[33][14] =    0.2294;
A[33][15] =   -0.1994;
A[33][16] =    1.7746;
A[33][17] =    1.0939;
A[33][18] =   -1.8407;
A[33][19] =    0.0144;
A[33][20] =    0.9094;
A[33][21] =   -1.8412;
A[33][22] =   -0.5093;
A[33][23] =   -2.1605;
A[33][24] =    0.4893;
A[33][25] =   -1.7901;
A[33][26] =   -0.7903;
A[33][27] =    2.0990;
A[33][28] =    3.5314;
A[33][29] =    0.0165;
A[33][30] =   -0.7832;
A[33][31] =   -1.6832;
A[33][32] =   -0.1680;
A[33][33] =   -1.7063;
A[33][34] =    2.1975;
A[33][35] =   -1.2548;
A[33][36] =    0.2768;
A[33][37] =   -0.1608;
A[33][38] =   -1.1207;
A[33][39] =   -0.3319;
A[33][40] =   -0.3322;
A[33][41] =   -0.2606;
A[33][42] =   -0.6625;
A[33][43] =    0.7278;
A[33][44] =   -0.7050;
A[34][0] =    0.3427;
A[34][1] =    0.8670;
A[34][2] =   -0.9475;
A[34][3] =    0.3830;
A[34][4] =   -0.4092;
A[34][5] =   -0.7052;
A[34][6] =   -0.1880;
A[34][7] =    0.4785;
A[34][8] =   -1.0064;
A[34][9] =   -1.0286;
A[34][10] =   -0.0870;
A[34][11] =   -0.1037;
A[34][12] =   -0.5520;
A[34][13] =    1.1568;
A[34][14] =   -0.3578;
A[34][15] =    0.1888;
A[34][16] =   -1.4710;
A[34][17] =   -1.8277;
A[34][18] =   -1.8853;
A[34][19] =    1.7813;
A[34][20] =   -1.4582;
A[34][21] =   -1.1748;
A[34][22] =    0.3425;
A[34][23] =   -0.3469;
A[34][24] =    2.1449;
A[34][25] =   -1.9056;
A[34][26] =    2.4450;
A[34][27] =    2.7737;
A[34][28] =    2.6727;
A[34][29] =   -0.8486;
A[34][30] =   -0.1157;
A[34][31] =   -3.6198;
A[34][32] =    1.1610;
A[34][33] =   -1.9166;
A[34][34] =   -0.4965;
A[34][35] =   -2.6459;
A[34][36] =   -0.1266;
A[34][37] =   -1.4259;
A[34][38] =   -1.9295;
A[34][39] =   -0.4274;
A[34][40] =    0.7214;
A[34][41] =   -0.0132;
A[34][42] =    0.0694;
A[34][43] =   -0.8118;
A[34][44] =    0.8402;
A[35][0] =    0.0713;
A[35][1] =   -0.4397;
A[35][2] =    0.9397;
A[35][3] =    0.7417;
A[35][4] =   -0.4172;
A[35][5] =    0.9388;
A[35][6] =    1.1645;
A[35][7] =   -0.0339;
A[35][8] =   -0.6933;
A[35][9] =   -1.2759;
A[35][10] =    1.3141;
A[35][11] =   -0.1153;
A[35][12] =   -1.0219;
A[35][13] =    0.1121;
A[35][14] =   -1.3405;
A[35][15] =    0.4958;
A[35][16] =    0.6264;
A[35][17] =    0.4494;
A[35][18] =   -0.6395;
A[35][19] =   -0.2076;
A[35][20] =    0.4962;
A[35][21] =    1.7749;
A[35][22] =   -2.0106;
A[35][23] =    1.3151;
A[35][24] =   -2.7567;
A[35][25] =   -0.9791;
A[35][26] =   -1.4829;
A[35][27] =   -1.0964;
A[35][28] =    1.1269;
A[35][29] =   -3.9040;
A[35][30] =    1.1406;
A[35][31] =   -4.1805;
A[35][32] =    3.5039;
A[35][33] =   -1.4211;
A[35][34] =    2.8195;
A[35][35] =    1.6552;
A[35][36] =   -0.2091;
A[35][37] =    0.5235;
A[35][38] =   -6.3247;
A[35][39] =   -0.5141;
A[35][40] =   -0.9994;
A[35][41] =    0.2574;
A[35][42] =    0.9572;
A[35][43] =    0.1237;
A[35][44] =    0.9436;
 double b[m];
b[0] =   -6.3586;
b[1] =    9.7385;
b[2] =   -5.7989;
b[3] =    1.3651;
b[4] =   -4.9619;
b[5] =    1.0959;
b[6] =   -4.2405;
b[7] =    9.7824;
b[8] =    9.2499;
b[9] =   -3.4872;
b[10] =   -2.4831;
b[11] =    3.9603;
b[12] =    3.9064;
b[13] =    6.6842;
b[14] =   -3.5936;
b[15] =    5.6790;
b[16] =    9.0734;
b[17] =   -0.2823;
b[18] =    3.9549;
b[19] =    5.4000;
b[20] =    4.8193;
b[21] =    8.3371;
b[22] =    3.5135;
b[23] =    8.9219;
b[24] =    7.0673;
b[25] =   10.8334;
b[26] =    6.6945;
b[27] =   11.6806;
b[28] =    3.0055;
b[29] =   -3.0762;
b[30] =   -0.9223;
b[31] =    1.7662;
b[32] =   -3.2614;
b[33] =   -1.4067;
b[34] =    6.4075;
b[35] =    6.3917;
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