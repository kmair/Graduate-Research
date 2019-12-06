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
 n = 40;
 m = 32;
 double x[n];
 FILE *file_input;
 file_input = fopen("input.in","r");
 for (i = 0; i < n; i++)
 {
 fscanf(file_input, "%lf\n", &x[i]);
 }
 fclose(file_input);
 
 double A[m][n];
A[0][0] =    0.6977;
A[0][1] =   -0.1620;
A[0][2] =   -0.4407;
A[0][3] =   -0.1828;
A[0][4] =   -0.8807;
A[0][5] =   -0.0790;
A[0][6] =    1.0128;
A[0][7] =    0.4781;
A[0][8] =    0.7999;
A[0][9] =   -0.7610;
A[0][10] =   -1.2409;
A[0][11] =    0.8914;
A[0][12] =    1.2765;
A[0][13] =    1.3439;
A[0][14] =   -0.6998;
A[0][15] =   -0.7913;
A[0][16] =    0.9981;
A[0][17] =    0.5814;
A[0][18] =    0.0158;
A[0][19] =    1.3507;
A[0][20] =   -0.1652;
A[0][21] =    0.6749;
A[0][22] =   -1.1966;
A[0][23] =    2.3400;
A[0][24] =    0.8355;
A[0][25] =   -0.6726;
A[0][26] =   -1.3692;
A[0][27] =    5.6526;
A[0][28] =   -4.2183;
A[0][29] =   -1.2926;
A[0][30] =   -0.0296;
A[0][31] =   -0.9603;
A[0][32] =   -0.1269;
A[0][33] =   -0.7209;
A[0][34] =   -0.5387;
A[0][35] =    0.9720;
A[0][36] =    0.0980;
A[0][37] =    0.7264;
A[0][38] =    0.5310;
A[0][39] =    0.0094;
A[1][0] =    0.3888;
A[1][1] =   -0.3317;
A[1][2] =    0.7908;
A[1][3] =   -0.6601;
A[1][4] =    0.0361;
A[1][5] =    0.4653;
A[1][6] =    0.2632;
A[1][7] =   -0.0807;
A[1][8] =    0.9621;
A[1][9] =   -1.2510;
A[1][10] =    0.9053;
A[1][11] =    0.2909;
A[1][12] =   -0.9312;
A[1][13] =   -1.3780;
A[1][14] =    0.0155;
A[1][15] =   -0.4119;
A[1][16] =    1.2975;
A[1][17] =    2.1355;
A[1][18] =   -2.8398;
A[1][19] =    3.2711;
A[1][20] =    2.7136;
A[1][21] =    1.3803;
A[1][22] =    0.9863;
A[1][23] =   -3.4698;
A[1][24] =   -3.5066;
A[1][25] =   -1.4941;
A[1][26] =   -2.2175;
A[1][27] =   -2.4393;
A[1][28] =    1.2739;
A[1][29] =   -1.0094;
A[1][30] =   -0.7320;
A[1][31] =   -0.6366;
A[1][32] =   -0.9057;
A[1][33] =   -0.5480;
A[1][34] =    0.8841;
A[1][35] =   -0.6059;
A[1][36] =    0.5207;
A[1][37] =    0.8132;
A[1][38] =    0.5445;
A[1][39] =   -0.2631;
A[2][0] =   -0.1994;
A[2][1] =    0.5582;
A[2][2] =    0.7034;
A[2][3] =    0.0890;
A[2][4] =   -0.5294;
A[2][5] =    0.4368;
A[2][6] =   -0.3689;
A[2][7] =   -0.3621;
A[2][8] =   -0.4381;
A[2][9] =   -0.2379;
A[2][10] =   -0.7036;
A[2][11] =    0.8552;
A[2][12] =   -1.4096;
A[2][13] =   -1.0756;
A[2][14] =   -0.6541;
A[2][15] =   -0.1687;
A[2][16] =    1.9724;
A[2][17] =   -0.9417;
A[2][18] =    1.2023;
A[2][19] =    1.9371;
A[2][20] =   -2.9511;
A[2][21] =   -0.5688;
A[2][22] =    2.6566;
A[2][23] =    1.3700;
A[2][24] =   -3.8995;
A[2][25] =   -1.7284;
A[2][26] =   -2.3980;
A[2][27] =   -4.6746;
A[2][28] =    3.2379;
A[2][29] =    3.4613;
A[2][30] =   -0.3923;
A[2][31] =   -0.8662;
A[2][32] =    0.3859;
A[2][33] =    0.0677;
A[2][34] =   -0.6741;
A[2][35] =    0.3621;
A[2][36] =    0.0429;
A[2][37] =    0.8138;
A[2][38] =    0.0106;
A[2][39] =    0.0115;
A[3][0] =    0.2405;
A[3][1] =   -0.4116;
A[3][2] =    0.5803;
A[3][3] =    0.5495;
A[3][4] =    0.0208;
A[3][5] =   -0.5969;
A[3][6] =    0.8601;
A[3][7] =   -0.6240;
A[3][8] =    0.6461;
A[3][9] =   -0.1136;
A[3][10] =    1.3255;
A[3][11] =   -0.9899;
A[3][12] =    1.6020;
A[3][13] =    0.5711;
A[3][14] =    1.4173;
A[3][15] =    1.5359;
A[3][16] =   -1.8193;
A[3][17] =    1.0128;
A[3][18] =   -2.7201;
A[3][19] =   -2.6410;
A[3][20] =   -1.3621;
A[3][21] =    2.1240;
A[3][22] =    2.8096;
A[3][23] =    1.1009;
A[3][24] =   -4.0812;
A[3][25] =    3.3212;
A[3][26] =   -4.3547;
A[3][27] =   -5.4615;
A[3][28] =    2.4758;
A[3][29] =    2.0536;
A[3][30] =   -0.5486;
A[3][31] =    0.0679;
A[3][32] =    0.6926;
A[3][33] =   -0.2903;
A[3][34] =   -0.7904;
A[3][35] =   -0.6843;
A[3][36] =    0.0253;
A[3][37] =   -0.7590;
A[3][38] =   -0.2571;
A[3][39] =    0.3616;
A[4][0] =    0.4761;
A[4][1] =    0.1803;
A[4][2] =   -0.1552;
A[4][3] =    0.1618;
A[4][4] =   -0.4594;
A[4][5] =   -0.2981;
A[4][6] =    0.9337;
A[4][7] =   -0.8599;
A[4][8] =   -0.3542;
A[4][9] =   -0.3026;
A[4][10] =    1.3906;
A[4][11] =   -1.1617;
A[4][12] =    0.5559;
A[4][13] =   -0.3422;
A[4][14] =   -0.8595;
A[4][15] =   -1.1319;
A[4][16] =   -2.5355;
A[4][17] =   -0.8343;
A[4][18] =    2.1009;
A[4][19] =    3.2190;
A[4][20] =    2.7430;
A[4][21] =    2.6480;
A[4][22] =   -3.2687;
A[4][23] =    3.1800;
A[4][24] =    2.2008;
A[4][25] =   -2.4857;
A[4][26] =   -2.2679;
A[4][27] =   -3.4472;
A[4][28] =   -2.0543;
A[4][29] =    4.5205;
A[4][30] =    0.2698;
A[4][31] =    0.4333;
A[4][32] =    0.5460;
A[4][33] =   -0.2297;
A[4][34] =    0.5687;
A[4][35] =    0.0571;
A[4][36] =    0.6794;
A[4][37] =   -0.6773;
A[4][38] =    0.8906;
A[4][39] =    0.6422;
A[5][0] =   -0.1312;
A[5][1] =    0.8005;
A[5][2] =   -0.8199;
A[5][3] =   -0.2379;
A[5][4] =    0.4612;
A[5][5] =    0.3785;
A[5][6] =   -0.1522;
A[5][7] =    1.0635;
A[5][8] =   -0.3410;
A[5][9] =   -0.0038;
A[5][10] =   -0.0525;
A[5][11] =   -1.4126;
A[5][12] =   -1.1780;
A[5][13] =   -1.8007;
A[5][14] =   -1.1152;
A[5][15] =   -0.7218;
A[5][16] =    0.4014;
A[5][17] =   -2.2917;
A[5][18] =   -2.0159;
A[5][19] =   -2.7266;
A[5][20] =    1.0589;
A[5][21] =    1.9468;
A[5][22] =    2.6537;
A[5][23] =   -2.0979;
A[5][24] =   -2.3975;
A[5][25] =    3.3046;
A[5][26] =    1.6941;
A[5][27] =    1.1327;
A[5][28] =   -6.1082;
A[5][29] =   -2.4805;
A[5][30] =    0.8404;
A[5][31] =   -0.6347;
A[5][32] =   -0.3460;
A[5][33] =    0.3326;
A[5][34] =    0.3956;
A[5][35] =    0.8744;
A[5][36] =    0.5345;
A[5][37] =    0.9360;
A[5][38] =    0.5002;
A[5][39] =   -0.8199;
A[6][0] =   -0.1859;
A[6][1] =    0.5729;
A[6][2] =   -0.1638;
A[6][3] =    0.1831;
A[6][4] =    0.3502;
A[6][5] =   -0.4893;
A[6][6] =    0.5930;
A[6][7] =    0.0782;
A[6][8] =    1.1954;
A[6][9] =    1.2087;
A[6][10] =   -0.6934;
A[6][11] =    0.5287;
A[6][12] =   -0.0310;
A[6][13] =   -1.3543;
A[6][14] =    1.3543;
A[6][15] =   -1.4805;
A[6][16] =    2.4820;
A[6][17] =   -2.1383;
A[6][18] =   -1.0053;
A[6][19] =    0.0008;
A[6][20] =   -0.3344;
A[6][21] =   -0.3966;
A[6][22] =   -1.0499;
A[6][23] =    1.1916;
A[6][24] =    0.9196;
A[6][25] =   -1.9258;
A[6][26] =    0.5629;
A[6][27] =   -1.7084;
A[6][28] =    3.7556;
A[6][29] =   -2.4411;
A[6][30] =    0.6443;
A[6][31] =    0.6294;
A[6][32] =   -0.1432;
A[6][33] =    0.9097;
A[6][34] =    0.5784;
A[6][35] =   -0.1861;
A[6][36] =   -0.1461;
A[6][37] =   -0.8826;
A[6][38] =    0.5044;
A[6][39] =   -0.9861;
A[7][0] =   -0.7218;
A[7][1] =   -0.3943;
A[7][2] =    0.5133;
A[7][3] =    0.8313;
A[7][4] =    0.0300;
A[7][5] =   -0.7765;
A[7][6] =    0.7840;
A[7][7] =    0.1650;
A[7][8] =    1.0150;
A[7][9] =    0.6833;
A[7][10] =   -0.0112;
A[7][11] =   -0.1785;
A[7][12] =   -0.5903;
A[7][13] =   -1.5554;
A[7][14] =    0.4335;
A[7][15] =    1.5794;
A[7][16] =   -0.9846;
A[7][17] =   -1.0726;
A[7][18] =   -1.7116;
A[7][19] =    3.1068;
A[7][20] =   -1.6471;
A[7][21] =   -1.0715;
A[7][22] =    0.5663;
A[7][23] =   -2.8460;
A[7][24] =   -2.7522;
A[7][25] =   -3.0720;
A[7][26] =    3.6069;
A[7][27] =    0.7424;
A[7][28] =   -1.8262;
A[7][29] =   -0.5689;
A[7][30] =    0.9116;
A[7][31] =    0.4961;
A[7][32] =   -0.6235;
A[7][33] =   -0.4077;
A[7][34] =   -0.6826;
A[7][35] =   -0.6989;
A[7][36] =   -0.0270;
A[7][37] =    0.4990;
A[7][38] =    0.1828;
A[7][39] =   -0.5734;
A[8][0] =   -0.3906;
A[8][1] =    0.4485;
A[8][2] =    0.3311;
A[8][3] =   -0.2467;
A[8][4] =   -0.0801;
A[8][5] =    0.8501;
A[8][6] =   -0.8235;
A[8][7] =   -0.2677;
A[8][8] =    0.7694;
A[8][9] =    0.8904;
A[8][10] =   -0.3865;
A[8][11] =    1.1138;
A[8][12] =   -0.7157;
A[8][13] =    1.1473;
A[8][14] =    1.7209;
A[8][15] =    0.1840;
A[8][16] =   -0.2764;
A[8][17] =    1.9530;
A[8][18] =   -2.7010;
A[8][19] =   -3.2533;
A[8][20] =    0.3196;
A[8][21] =   -1.9960;
A[8][22] =    0.8972;
A[8][23] =   -1.6244;
A[8][24] =   -1.7601;
A[8][25] =   -4.0370;
A[8][26] =    3.4809;
A[8][27] =    5.6592;
A[8][28] =   -2.8352;
A[8][29] =    1.0463;
A[8][30] =    0.0843;
A[8][31] =    0.1213;
A[8][32] =    0.5606;
A[8][33] =    0.3315;
A[8][34] =    0.4648;
A[8][35] =   -0.2442;
A[8][36] =   -0.4831;
A[8][37] =    0.2029;
A[8][38] =    0.5942;
A[8][39] =   -0.6173;
A[9][0] =    0.4363;
A[9][1] =   -0.4980;
A[9][2] =   -0.0646;
A[9][3] =   -0.1000;
A[9][4] =   -0.7567;
A[9][5] =   -0.8427;
A[9][6] =    0.6270;
A[9][7] =   -0.8890;
A[9][8] =    0.0793;
A[9][9] =    0.2802;
A[9][10] =    1.1660;
A[9][11] =   -0.0439;
A[9][12] =   -1.3936;
A[9][13] =   -0.7664;
A[9][14] =   -0.1321;
A[9][15] =    1.9472;
A[9][16] =   -0.8770;
A[9][17] =    2.1120;
A[9][18] =    0.9741;
A[9][19] =    2.3271;
A[9][20] =    2.4758;
A[9][21] =    1.8984;
A[9][22] =   -2.2369;
A[9][23] =    1.2543;
A[9][24] =    2.2616;
A[9][25] =   -1.6209;
A[9][26] =   -1.9389;
A[9][27] =   -4.6015;
A[9][28] =    7.8498;
A[9][29] =   -2.4312;
A[9][30] =    0.4955;
A[9][31] =    0.8466;
A[9][32] =   -0.1850;
A[9][33] =   -0.4252;
A[9][34] =   -0.2316;
A[9][35] =    0.7152;
A[9][36] =   -0.5283;
A[9][37] =   -0.2212;
A[9][38] =    0.6087;
A[9][39] =    0.5049;
A[10][0] =   -0.5819;
A[10][1] =   -0.1006;
A[10][2] =    0.6715;
A[10][3] =    0.6720;
A[10][4] =    0.6291;
A[10][5] =    0.2919;
A[10][6] =    0.2227;
A[10][7] =    1.0663;
A[10][8] =   -0.5549;
A[10][9] =    1.1925;
A[10][10] =    0.5994;
A[10][11] =    1.3894;
A[10][12] =   -1.4099;
A[10][13] =    0.0667;
A[10][14] =   -1.5688;
A[10][15] =   -1.7507;
A[10][16] =    0.6599;
A[10][17] =   -1.6000;
A[10][18] =    0.9322;
A[10][19] =   -0.2316;
A[10][20] =    3.2690;
A[10][21] =   -1.4322;
A[10][22] =    1.4941;
A[10][23] =   -2.9779;
A[10][24] =   -3.8226;
A[10][25] =   -2.5145;
A[10][26] =   -3.1253;
A[10][27] =   -5.9486;
A[10][28] =    3.6648;
A[10][29] =    1.6093;
A[10][30] =    0.6780;
A[10][31] =    0.3180;
A[10][32] =    0.9838;
A[10][33] =   -0.4250;
A[10][34] =   -0.2485;
A[10][35] =   -0.3627;
A[10][36] =    0.2161;
A[10][37] =   -0.9854;
A[10][38] =   -0.5984;
A[10][39] =    0.8577;
A[11][0] =    0.6078;
A[11][1] =   -0.0177;
A[11][2] =   -0.6467;
A[11][3] =    0.2489;
A[11][4] =    0.3026;
A[11][5] =    0.3198;
A[11][6] =   -1.1580;
A[11][7] =    0.2698;
A[11][8] =    0.4727;
A[11][9] =    1.2153;
A[11][10] =    0.6552;
A[11][11] =    0.2447;
A[11][12] =    0.1487;
A[11][13] =   -1.6174;
A[11][14] =   -0.3481;
A[11][15] =   -0.6866;
A[11][16] =   -1.8612;
A[11][17] =    1.4793;
A[11][18] =   -0.1805;
A[11][19] =    2.7532;
A[11][20] =    2.5660;
A[11][21] =    2.6422;
A[11][22] =    3.6912;
A[11][23] =   -1.1949;
A[11][24] =   -1.9096;
A[11][25] =   -1.3058;
A[11][26] =   -4.5099;
A[11][27] =    4.7636;
A[11][28] =    1.8618;
A[11][29] =    0.1015;
A[11][30] =   -0.3750;
A[11][31] =    0.9260;
A[11][32] =    0.8722;
A[11][33] =   -0.0928;
A[11][34] =   -0.4964;
A[11][35] =   -0.4890;
A[11][36] =    0.5029;
A[11][37] =    0.7297;
A[11][38] =    0.2932;
A[11][39] =    0.4148;
A[12][0] =    0.4843;
A[12][1] =    0.5656;
A[12][2] =   -0.7104;
A[12][3] =   -0.8442;
A[12][4] =    0.4484;
A[12][5] =   -0.2188;
A[12][6] =   -0.8864;
A[12][7] =   -0.1696;
A[12][8] =    1.2339;
A[12][9] =   -0.7576;
A[12][10] =   -0.0369;
A[12][11] =    0.8980;
A[12][12] =    1.2536;
A[12][13] =   -1.7516;
A[12][14] =    0.1955;
A[12][15] =    1.8950;
A[12][16] =    1.1926;
A[12][17] =    0.3315;
A[12][18] =    2.2247;
A[12][19] =   -0.7118;
A[12][20] =    3.1483;
A[12][21] =   -2.0520;
A[12][22] =    0.3534;
A[12][23] =   -2.4957;
A[12][24] =   -0.5242;
A[12][25] =    1.2331;
A[12][26] =   -2.5396;
A[12][27] =   -1.5289;
A[12][28] =    5.5258;
A[12][29] =   -4.9333;
A[12][30] =   -0.5010;
A[12][31] =   -0.7675;
A[12][32] =    0.1167;
A[12][33] =    0.7332;
A[12][34] =   -0.1617;
A[12][35] =    0.9572;
A[12][36] =   -0.7654;
A[12][37] =    0.3206;
A[12][38] =    0.7448;
A[12][39] =   -0.1547;
A[13][0] =    0.5023;
A[13][1] =   -0.7615;
A[13][2] =    0.0789;
A[13][3] =    0.4801;
A[13][4] =   -0.0990;
A[13][5] =    0.0426;
A[13][6] =   -0.9178;
A[13][7] =    1.2002;
A[13][8] =   -0.4397;
A[13][9] =   -1.3534;
A[13][10] =    1.3066;
A[13][11] =    0.1249;
A[13][12] =   -0.8010;
A[13][13] =   -0.6572;
A[13][14] =   -0.9580;
A[13][15] =    0.8031;
A[13][16] =    1.8235;
A[13][17] =   -2.6154;
A[13][18] =    2.6400;
A[13][19] =    2.6902;
A[13][20] =    1.3730;
A[13][21] =    1.0480;
A[13][22] =   -1.6045;
A[13][23] =   -2.8570;
A[13][24] =   -4.1330;
A[13][25] =    4.4074;
A[13][26] =    3.6416;
A[13][27] =    1.0641;
A[13][28] =   -0.2952;
A[13][29] =    0.0150;
A[13][30] =   -0.5968;
A[13][31] =   -0.7894;
A[13][32] =   -0.9655;
A[13][33] =    0.6698;
A[13][34] =    0.7924;
A[13][35] =    0.4244;
A[13][36] =    0.3943;
A[13][37] =    0.9102;
A[13][38] =   -0.0384;
A[13][39] =   -0.5054;
A[14][0] =    0.0071;
A[14][1] =    0.5270;
A[14][2] =   -0.4103;
A[14][3] =    0.6647;
A[14][4] =    0.5989;
A[14][5] =    0.7778;
A[14][6] =   -0.0337;
A[14][7] =   -0.8180;
A[14][8] =    0.3239;
A[14][9] =    0.9282;
A[14][10] =    1.1515;
A[14][11] =   -0.7920;
A[14][12] =    0.7890;
A[14][13] =   -0.7113;
A[14][14] =    0.3238;
A[14][15] =    0.2883;
A[14][16] =   -0.4792;
A[14][17] =    1.8256;
A[14][18] =    0.7815;
A[14][19] =    1.2711;
A[14][20] =    2.5037;
A[14][21] =    3.1747;
A[14][22] =   -2.7886;
A[14][23] =    0.2820;
A[14][24] =    2.4403;
A[14][25] =    0.5106;
A[14][26] =   -3.8664;
A[14][27] =    0.0112;
A[14][28] =    1.7159;
A[14][29] =   -3.9332;
A[14][30] =    0.9967;
A[14][31] =    0.5728;
A[14][32] =    0.7379;
A[14][33] =    0.0351;
A[14][34] =   -0.0640;
A[14][35] =   -0.8716;
A[14][36] =    0.8897;
A[14][37] =   -0.4316;
A[14][38] =   -0.9845;
A[14][39] =    0.5385;
A[15][0] =   -0.5776;
A[15][1] =   -0.2803;
A[15][2] =    0.7426;
A[15][3] =   -0.4015;
A[15][4] =   -0.1035;
A[15][5] =   -0.5709;
A[15][6] =   -1.0092;
A[15][7] =    0.9397;
A[15][8] =    0.3936;
A[15][9] =   -1.1975;
A[15][10] =   -0.7450;
A[15][11] =   -0.3222;
A[15][12] =   -0.6692;
A[15][13] =   -1.1533;
A[15][14] =   -0.3656;
A[15][15] =    1.0890;
A[15][16] =   -1.4539;
A[15][17] =    1.5093;
A[15][18] =    2.1066;
A[15][19] =   -0.9346;
A[15][20] =   -0.2955;
A[15][21] =    3.2735;
A[15][22] =   -1.8843;
A[15][23] =    2.8441;
A[15][24] =   -0.1111;
A[15][25] =    3.0707;
A[15][26] =   -0.9971;
A[15][27] =    2.1524;
A[15][28] =   -0.4043;
A[15][29] =    1.9247;
A[15][30] =    0.8682;
A[15][31] =   -0.6407;
A[15][32] =    0.0019;
A[15][33] =    0.5927;
A[15][34] =    0.7064;
A[15][35] =   -0.1446;
A[15][36] =    0.5513;
A[15][37] =    0.5829;
A[15][38] =    0.1398;
A[15][39] =    0.1194;
A[16][0] =   -0.5724;
A[16][1] =    0.7894;
A[16][2] =    0.1017;
A[16][3] =   -0.4871;
A[16][4] =   -0.7201;
A[16][5] =   -0.8992;
A[16][6] =    0.7548;
A[16][7] =    0.2078;
A[16][8] =    0.8393;
A[16][9] =   -0.7684;
A[16][10] =    0.1972;
A[16][11] =   -0.8356;
A[16][12] =   -0.9488;
A[16][13] =    1.1248;
A[16][14] =    0.8239;
A[16][15] =   -0.7814;
A[16][16] =   -1.3178;
A[16][17] =   -0.7792;
A[16][18] =    1.3455;
A[16][19] =   -1.7426;
A[16][20] =    1.6266;
A[16][21] =   -2.9531;
A[16][22] =    1.9547;
A[16][23] =   -1.6984;
A[16][24] =    0.2524;
A[16][25] =    1.4625;
A[16][26] =    1.0705;
A[16][27] =    4.2340;
A[16][28] =    2.6484;
A[16][29] =    4.6747;
A[16][30] =   -0.9216;
A[16][31] =   -0.8176;
A[16][32] =   -0.6326;
A[16][33] =   -0.7190;
A[16][34] =   -0.8753;
A[16][35] =    0.1598;
A[16][36] =    0.8745;
A[16][37] =   -0.6570;
A[16][38] =   -0.7338;
A[16][39] =    0.5104;
A[17][0] =   -0.5609;
A[17][1] =   -0.0854;
A[17][2] =   -0.4997;
A[17][3] =   -0.0631;
A[17][4] =   -0.1333;
A[17][5] =   -0.1794;
A[17][6] =   -0.8976;
A[17][7] =    0.1538;
A[17][8] =   -0.7997;
A[17][9] =    0.0005;
A[17][10] =   -0.8936;
A[17][11] =   -0.3448;
A[17][12] =   -0.6714;
A[17][13] =   -1.3482;
A[17][14] =   -1.9651;
A[17][15] =   -0.7541;
A[17][16] =    0.5891;
A[17][17] =   -0.6540;
A[17][18] =   -1.3354;
A[17][19] =    2.2521;
A[17][20] =    2.7882;
A[17][21] =    0.3764;
A[17][22] =    0.9966;
A[17][23] =   -3.6345;
A[17][24] =   -1.6221;
A[17][25] =    4.2949;
A[17][26] =   -3.7484;
A[17][27] =   -0.7537;
A[17][28] =   -0.0127;
A[17][29] =    4.4340;
A[17][30] =    0.5602;
A[17][31] =   -0.4652;
A[17][32] =    0.4750;
A[17][33] =   -0.5953;
A[17][34] =    0.5672;
A[17][35] =   -0.4720;
A[17][36] =    0.8446;
A[17][37] =    0.9787;
A[17][38] =    0.4113;
A[17][39] =    0.1674;
A[18][0] =    0.5131;
A[18][1] =    0.2742;
A[18][2] =    0.1441;
A[18][3] =    0.2804;
A[18][4] =   -0.8353;
A[18][5] =   -0.3894;
A[18][6] =   -0.7431;
A[18][7] =   -1.1795;
A[18][8] =    0.8400;
A[18][9] =    0.8550;
A[18][10] =   -0.0953;
A[18][11] =    0.2985;
A[18][12] =   -0.2870;
A[18][13] =    0.6011;
A[18][14] =    0.9691;
A[18][15] =   -1.5722;
A[18][16] =    2.4772;
A[18][17] =    0.0638;
A[18][18] =    1.1594;
A[18][19] =   -1.8486;
A[18][20] =    2.4949;
A[18][21] =   -2.7179;
A[18][22] =    2.6663;
A[18][23] =   -0.1501;
A[18][24] =   -0.9305;
A[18][25] =   -3.1530;
A[18][26] =    0.9568;
A[18][27] =    6.1337;
A[18][28] =    3.7303;
A[18][29] =    0.0966;
A[18][30] =    0.8605;
A[18][31] =   -0.3579;
A[18][32] =    0.2375;
A[18][33] =   -0.2052;
A[18][34] =    0.7627;
A[18][35] =   -0.2876;
A[18][36] =   -0.0040;
A[18][37] =   -0.5367;
A[18][38] =   -0.0098;
A[18][39] =    0.1996;
A[19][0] =   -0.0966;
A[19][1] =   -0.4966;
A[19][2] =    0.3512;
A[19][3] =    0.6820;
A[19][4] =   -0.4356;
A[19][5] =    0.5428;
A[19][6] =    0.3405;
A[19][7] =    0.0240;
A[19][8] =    0.0567;
A[19][9] =   -0.1197;
A[19][10] =    1.1306;
A[19][11] =   -0.0311;
A[19][12] =   -0.7319;
A[19][13] =   -1.1324;
A[19][14] =    1.5705;
A[19][15] =    0.8327;
A[19][16] =   -2.3066;
A[19][17] =    2.2819;
A[19][18] =   -0.3438;
A[19][19] =    1.0036;
A[19][20] =   -1.8256;
A[19][21] =    1.7700;
A[19][22] =    1.9155;
A[19][23] =    2.2249;
A[19][24] =    3.1605;
A[19][25] =    1.8861;
A[19][26] =    2.5151;
A[19][27] =   -1.7056;
A[19][28] =    2.9540;
A[19][29] =    0.3317;
A[19][30] =   -0.9161;
A[19][31] =   -0.1442;
A[19][32] =    0.6983;
A[19][33] =    0.6280;
A[19][34] =    0.0832;
A[19][35] =    0.5241;
A[19][36] =    0.6184;
A[19][37] =    0.0132;
A[19][38] =   -0.7540;
A[19][39] =    0.8502;
A[20][0] =    0.1626;
A[20][1] =    0.5589;
A[20][2] =   -0.3463;
A[20][3] =   -0.4265;
A[20][4] =    0.5080;
A[20][5] =   -0.5882;
A[20][6] =    0.4880;
A[20][7] =    0.5630;
A[20][8] =   -0.8174;
A[20][9] =    0.6615;
A[20][10] =    0.3172;
A[20][11] =   -0.0234;
A[20][12] =    0.2225;
A[20][13] =   -1.5785;
A[20][14] =    0.3991;
A[20][15] =   -0.3294;
A[20][16] =    2.1201;
A[20][17] =   -1.3377;
A[20][18] =   -0.2156;
A[20][19] =    0.4444;
A[20][20] =    0.4463;
A[20][21] =   -1.0897;
A[20][22] =   -2.9196;
A[20][23] =   -2.3798;
A[20][24] =   -1.3543;
A[20][25] =   -2.4760;
A[20][26] =    1.9754;
A[20][27] =   -3.0566;
A[20][28] =   -0.0295;
A[20][29] =   -3.7057;
A[20][30] =    0.1740;
A[20][31] =    0.3016;
A[20][32] =   -0.0856;
A[20][33] =    0.7847;
A[20][34] =    0.4250;
A[20][35] =    0.6017;
A[20][36] =   -0.4956;
A[20][37] =   -0.8483;
A[20][38] =   -0.1462;
A[20][39] =   -0.0481;
A[21][0] =   -0.2651;
A[21][1] =   -0.7250;
A[21][2] =    0.2123;
A[21][3] =   -0.2008;
A[21][4] =    0.5339;
A[21][5] =    0.4744;
A[21][6] =    0.7674;
A[21][7] =   -0.1574;
A[21][8] =   -0.3238;
A[21][9] =    0.4608;
A[21][10] =   -0.1076;
A[21][11] =    1.4656;
A[21][12] =    1.1591;
A[21][13] =    0.4293;
A[21][14] =   -0.3410;
A[21][15] =   -0.3731;
A[21][16] =   -0.1590;
A[21][17] =    2.5193;
A[21][18] =    2.4329;
A[21][19] =    1.2758;
A[21][20] =    1.2521;
A[21][21] =   -0.4437;
A[21][22] =   -3.1352;
A[21][23] =   -2.2892;
A[21][24] =   -2.9042;
A[21][25] =   -1.0569;
A[21][26] =   -1.0480;
A[21][27] =   -3.0514;
A[21][28] =   -6.1574;
A[21][29] =    3.7966;
A[21][30] =   -0.3217;
A[21][31] =    0.0118;
A[21][32] =    0.3384;
A[21][33] =   -0.9893;
A[21][34] =   -0.8380;
A[21][35] =   -0.4173;
A[21][36] =    0.5945;
A[21][37] =   -0.0439;
A[21][38] =    0.3056;
A[21][39] =   -0.9005;
A[22][0] =    0.0846;
A[22][1] =    0.6225;
A[22][2] =   -0.0786;
A[22][3] =    0.5875;
A[22][4] =   -0.2800;
A[22][5] =    0.1461;
A[22][6] =   -0.5451;
A[22][7] =   -0.2713;
A[22][8] =    0.4499;
A[22][9] =    1.0763;
A[22][10] =    0.9238;
A[22][11] =   -0.6423;
A[22][12] =   -0.7798;
A[22][13] =   -1.0333;
A[22][14] =    0.4146;
A[22][15] =    2.1396;
A[22][16] =    1.6682;
A[22][17] =    1.2040;
A[22][18] =   -2.7407;
A[22][19] =    1.7140;
A[22][20] =    1.3665;
A[22][21] =   -1.5725;
A[22][22] =   -1.8126;
A[22][23] =    3.4359;
A[22][24] =    3.0278;
A[22][25] =   -2.7865;
A[22][26] =   -4.0165;
A[22][27] =   -2.8225;
A[22][28] =   -0.6384;
A[22][29] =   -4.6799;
A[22][30] =   -0.7785;
A[22][31] =    0.2853;
A[22][32] =   -0.6612;
A[22][33] =   -0.7126;
A[22][34] =   -0.4079;
A[22][35] =    0.0942;
A[22][36] =   -0.0036;
A[22][37] =   -0.3814;
A[22][38] =   -0.9500;
A[22][39] =   -0.8896;
A[23][0] =    0.3060;
A[23][1] =    0.4026;
A[23][2] =    0.3204;
A[23][3] =   -0.5096;
A[23][4] =    0.7005;
A[23][5] =   -0.3610;
A[23][6] =   -0.6745;
A[23][7] =    1.0148;
A[23][8] =   -0.4630;
A[23][9] =   -0.1096;
A[23][10] =   -0.3031;
A[23][11] =    0.2979;
A[23][12] =    1.3300;
A[23][13] =    1.3475;
A[23][14] =   -1.6434;
A[23][15] =   -0.3195;
A[23][16] =    0.1483;
A[23][17] =    1.8761;
A[23][18] =    0.6232;
A[23][19] =    3.1671;
A[23][20] =   -2.7999;
A[23][21] =    2.4851;
A[23][22] =    3.5200;
A[23][23] =    3.3270;
A[23][24] =   -2.1108;
A[23][25] =    0.9005;
A[23][26] =   -2.4950;
A[23][27] =    0.6380;
A[23][28] =    7.8463;
A[23][29] =   -0.9115;
A[23][30] =    0.2062;
A[23][31] =    0.3057;
A[23][32] =    0.9032;
A[23][33] =    0.2760;
A[23][34] =   -0.5123;
A[23][35] =    0.2588;
A[23][36] =    0.5884;
A[23][37] =    0.7410;
A[23][38] =    0.7277;
A[23][39] =    0.2549;
A[24][0] =    0.2231;
A[24][1] =    0.5720;
A[24][2] =    0.4104;
A[24][3] =    0.1357;
A[24][4] =    0.1331;
A[24][5] =   -0.3532;
A[24][6] =    0.2112;
A[24][7] =    0.7949;
A[24][8] =   -1.2105;
A[24][9] =   -0.4729;
A[24][10] =    1.0788;
A[24][11] =   -0.7196;
A[24][12] =   -0.6846;
A[24][13] =   -0.9947;
A[24][14] =    1.8096;
A[24][15] =   -1.7182;
A[24][16] =    1.1736;
A[24][17] =    1.3465;
A[24][18] =   -0.1074;
A[24][19] =   -0.2936;
A[24][20] =    2.4365;
A[24][21] =    2.1699;
A[24][22] =   -1.0342;
A[24][23] =   -0.4196;
A[24][24] =   -3.6042;
A[24][25] =    1.2958;
A[24][26] =    1.0597;
A[24][27] =   -3.7393;
A[24][28] =   -0.9017;
A[24][29] =   -4.8698;
A[24][30] =    0.6304;
A[24][31] =   -0.1676;
A[24][32] =   -0.1093;
A[24][33] =    0.5817;
A[24][34] =   -0.9873;
A[24][35] =    0.0296;
A[24][36] =    0.0869;
A[24][37] =   -0.7989;
A[24][38] =   -0.4849;
A[24][39] =    0.4397;
A[25][0] =    0.3710;
A[25][1] =   -0.1867;
A[25][2] =   -0.4014;
A[25][3] =   -0.7282;
A[25][4] =   -0.7056;
A[25][5] =    0.0212;
A[25][6] =   -0.8044;
A[25][7] =   -0.6346;
A[25][8] =    0.5945;
A[25][9] =   -0.0445;
A[25][10] =   -0.9565;
A[25][11] =    1.1636;
A[25][12] =    1.4094;
A[25][13] =    0.1416;
A[25][14] =    1.1854;
A[25][15] =    1.8407;
A[25][16] =   -2.3734;
A[25][17] =   -2.5448;
A[25][18] =    2.0714;
A[25][19] =   -2.5031;
A[25][20] =   -1.9089;
A[25][21] =   -3.0733;
A[25][22] =    1.8310;
A[25][23] =    0.6786;
A[25][24] =   -2.7787;
A[25][25] =   -3.5581;
A[25][26] =    2.1821;
A[25][27] =   -1.1113;
A[25][28] =   -8.1007;
A[25][29] =   -0.1805;
A[25][30] =    0.3707;
A[25][31] =    0.2427;
A[25][32] =   -0.5694;
A[25][33] =    0.1333;
A[25][34] =   -0.0757;
A[25][35] =   -0.3605;
A[25][36] =    0.8070;
A[25][37] =    0.3924;
A[25][38] =    0.1108;
A[25][39] =    0.6260;
A[26][0] =   -0.0725;
A[26][1] =   -0.1241;
A[26][2] =    0.2118;
A[26][3] =   -0.3770;
A[26][4] =    0.1297;
A[26][5] =   -0.6692;
A[26][6] =   -1.1838;
A[26][7] =    0.7237;
A[26][8] =   -1.2695;
A[26][9] =   -0.8807;
A[26][10] =   -0.8901;
A[26][11] =   -0.8171;
A[26][12] =   -1.3439;
A[26][13] =    0.3340;
A[26][14] =   -1.4612;
A[26][15] =    1.8269;
A[26][16] =   -0.5460;
A[26][17] =   -1.1623;
A[26][18] =   -1.4813;
A[26][19] =    2.7391;
A[26][20] =    2.2924;
A[26][21] =   -1.4346;
A[26][22] =    2.7769;
A[26][23] =   -1.3760;
A[26][24] =   -0.1619;
A[26][25] =   -1.4199;
A[26][26] =   -4.6288;
A[26][27] =   -2.2009;
A[26][28] =   -0.8569;
A[26][29] =    4.6646;
A[26][30] =   -0.2915;
A[26][31] =   -0.9379;
A[26][32] =   -0.6066;
A[26][33] =    0.4777;
A[26][34] =    0.6441;
A[26][35] =   -0.1809;
A[26][36] =    0.1800;
A[26][37] =    0.0393;
A[26][38] =    0.9159;
A[26][39] =   -0.2669;
A[27][0] =   -0.4712;
A[27][1] =    0.5329;
A[27][2] =   -0.4773;
A[27][3] =   -0.2223;
A[27][4] =   -0.5265;
A[27][5] =   -0.5864;
A[27][6] =   -0.4028;
A[27][7] =   -0.4148;
A[27][8] =   -0.8186;
A[27][9] =    0.3475;
A[27][10] =    1.1526;
A[27][11] =   -1.4242;
A[27][12] =    0.8668;
A[27][13] =   -0.5871;
A[27][14] =    0.6941;
A[27][15] =   -1.4487;
A[27][16] =    0.4518;
A[27][17] =   -1.4854;
A[27][18] =   -2.6157;
A[27][19] =   -1.4124;
A[27][20] =   -0.4317;
A[27][21] =    2.3101;
A[27][22] =    1.0777;
A[27][23] =   -0.7413;
A[27][24] =   -4.0065;
A[27][25] =    1.3201;
A[27][26] =   -4.0703;
A[27][27] =   -5.1526;
A[27][28] =   -5.0133;
A[27][29] =    3.0228;
A[27][30] =   -0.5562;
A[27][31] =    0.9762;
A[27][32] =   -0.1098;
A[27][33] =   -0.3408;
A[27][34] =   -0.4647;
A[27][35] =   -0.8151;
A[27][36] =    0.4654;
A[27][37] =   -0.9752;
A[27][38] =    0.4482;
A[27][39] =    0.3992;
A[28][0] =   -0.0361;
A[28][1] =    0.7043;
A[28][2] =   -0.7273;
A[28][3] =    0.2631;
A[28][4] =   -0.5768;
A[28][5] =    0.6066;
A[28][6] =    1.1652;
A[28][7] =    1.2080;
A[28][8] =    1.1267;
A[28][9] =   -1.3585;
A[28][10] =   -0.9732;
A[28][11] =   -1.3852;
A[28][12] =    0.2418;
A[28][13] =    1.2286;
A[28][14] =    1.5303;
A[28][15] =    0.9626;
A[28][16] =    0.2073;
A[28][17] =   -2.1227;
A[28][18] =   -0.6117;
A[28][19] =    2.9953;
A[28][20] =   -1.6869;
A[28][21] =   -2.9371;
A[28][22] =    1.3001;
A[28][23] =    3.4837;
A[28][24] =    2.0893;
A[28][25] =   -1.7298;
A[28][26] =    0.2134;
A[28][27] =    5.8322;
A[28][28] =   -5.3008;
A[28][29] =    0.8872;
A[28][30] =   -0.9929;
A[28][31] =    0.9728;
A[28][32] =    0.5462;
A[28][33] =   -0.1865;
A[28][34] =    0.0497;
A[28][35] =    0.4957;
A[28][36] =    0.5614;
A[28][37] =   -0.5497;
A[28][38] =   -0.2016;
A[28][39] =   -0.0000;
A[29][0] =    0.2944;
A[29][1] =    0.3491;
A[29][2] =    0.3740;
A[29][3] =    0.4465;
A[29][4] =   -0.6339;
A[29][5] =    0.2264;
A[29][6] =    0.0554;
A[29][7] =    0.6554;
A[29][8] =   -1.1532;
A[29][9] =   -0.5344;
A[29][10] =    0.5721;
A[29][11] =    1.3386;
A[29][12] =   -0.7517;
A[29][13] =   -1.5761;
A[29][14] =    1.5632;
A[29][15] =   -1.2910;
A[29][16] =   -0.7847;
A[29][17] =   -2.0703;
A[29][18] =    0.7211;
A[29][19] =    2.1878;
A[29][20] =    1.5175;
A[29][21] =   -2.7338;
A[29][22] =    1.4069;
A[29][23] =    0.8040;
A[29][24] =    3.9128;
A[29][25] =    3.5490;
A[29][26] =   -1.9662;
A[29][27] =    5.2278;
A[29][28] =    6.4697;
A[29][29] =    2.3517;
A[29][30] =   -0.1619;
A[29][31] =   -0.9117;
A[29][32] =   -0.5053;
A[29][33] =    0.7347;
A[29][34] =    0.0949;
A[29][35] =   -0.6946;
A[29][36] =   -0.5261;
A[29][37] =    0.9487;
A[29][38] =   -0.3553;
A[29][39] =   -0.5562;
A[30][0] =    0.7493;
A[30][1] =   -0.7059;
A[30][2] =    0.1195;
A[30][3] =   -0.4585;
A[30][4] =   -0.7314;
A[30][5] =   -0.8057;
A[30][6] =   -0.3800;
A[30][7] =    0.6074;
A[30][8] =    0.1186;
A[30][9] =   -0.3579;
A[30][10] =    0.0367;
A[30][11] =   -0.5856;
A[30][12] =   -1.2589;
A[30][13] =   -1.0604;
A[30][14] =    1.1422;
A[30][15] =   -0.4324;
A[30][16] =   -0.9473;
A[30][17] =   -1.3182;
A[30][18] =    0.0771;
A[30][19] =    3.2476;
A[30][20] =    0.7388;
A[30][21] =   -0.2128;
A[30][22] =   -1.6727;
A[30][23] =   -2.9457;
A[30][24] =   -2.8764;
A[30][25] =   -2.2616;
A[30][26] =    3.3712;
A[30][27] =    5.3715;
A[30][28] =   -2.5199;
A[30][29] =   -3.0767;
A[30][30] =    0.8069;
A[30][31] =   -0.0478;
A[30][32] =   -0.8735;
A[30][33] =   -0.7450;
A[30][34] =    0.0629;
A[30][35] =   -0.9646;
A[30][36] =    0.9430;
A[30][37] =    0.2723;
A[30][38] =   -0.1703;
A[30][39] =    0.1891;
A[31][0] =    0.1827;
A[31][1] =    0.3144;
A[31][2] =   -0.5883;
A[31][3] =    0.1033;
A[31][4] =   -0.7903;
A[31][5] =    0.2925;
A[31][6] =   -1.1876;
A[31][7] =    0.0593;
A[31][8] =   -1.1657;
A[31][9] =    1.1476;
A[31][10] =    0.2400;
A[31][11] =   -0.7784;
A[31][12] =    1.6101;
A[31][13] =    1.6854;
A[31][14] =    1.1181;
A[31][15] =    1.3551;
A[31][16] =   -0.9121;
A[31][17] =   -0.4614;
A[31][18] =    2.6660;
A[31][19] =   -2.2105;
A[31][20] =   -0.2550;
A[31][21] =    0.8307;
A[31][22] =   -1.5443;
A[31][23] =    2.7928;
A[31][24] =    1.1407;
A[31][25] =    2.4688;
A[31][26] =   -0.8902;
A[31][27] =   -0.7782;
A[31][28] =   -7.4175;
A[31][29] =   -1.4974;
A[31][30] =    0.2176;
A[31][31] =    0.6082;
A[31][32] =    0.8077;
A[31][33] =   -0.5086;
A[31][34] =   -0.3941;
A[31][35] =   -0.0659;
A[31][36] =    0.5517;
A[31][37] =    0.5893;
A[31][38] =    0.5246;
A[31][39] =    0.0204;
 double b[m];
b[0] =   -3.7206;
b[1] =    8.0736;
b[2] =   11.1658;
b[3] =    7.3684;
b[4] =   11.4859;
b[5] =   -6.5718;
b[6] =    5.1189;
b[7] =   -4.2028;
b[8] =  -13.5273;
b[9] =   17.1250;
b[10] =   10.4157;
b[11] =    9.3207;
b[12] =   11.6735;
b[13] =    3.7889;
b[14] =    8.6109;
b[15] =    3.1727;
b[16] =   -4.1678;
b[17] =    3.2184;
b[18] =   -1.0776;
b[19] =    0.5575;
b[20] =    2.6353;
b[21] =   -1.5116;
b[22] =    8.1992;
b[23] =   12.6984;
b[24] =    5.4995;
b[25] =   -5.5596;
b[26] =    7.3786;
b[27] =    4.3751;
b[28] =   -5.6002;
b[29] =    4.0603;
b[30] =   -3.4428;
b[31] =   -2.0505;
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